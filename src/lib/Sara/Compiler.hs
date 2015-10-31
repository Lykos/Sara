module Sara.Compiler (
  run
  , compile) where

import Sara.Parser
import Sara.TypeChecker
import Sara.Syntax
import Sara.CodeGenerator
import Sara.Errors
import Sara.Reporter

import Control.Monad.Identity
import Control.Monad.Except
import Data.Int
import Foreign.Ptr ( FunPtr, castFunPtr )

import LLVM.General.PassManager
import LLVM.General.AST
import LLVM.General.Context

import qualified LLVM.General.Module as M
import qualified LLVM.General.ExecutionEngine as EE

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

type ErrorOrIO a = ExceptT Error IO a

codegenStage :: (Context -> M.Module -> IO (Either Error a)) -> Module -> Program -> ErrorOrIO a
codegenStage report modl prog = ExceptT $ withContext $ \context -> codegen' context (codegen modl prog)
  where codegen' context modl' = flattenError $ runExceptT $ M.withModuleFromAST context modl' $ generate context
        generate context m = withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          report context m

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int64) -> IO Int64

runFn :: FunPtr a -> IO Int64
runFn fn = haskFun (castFunPtr fn :: FunPtr (IO Int64))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

runStage :: Context -> M.Module -> IO (Either Error Int64)
runStage context mod = jit context $ \engine -> EE.withModuleInEngine engine mod execute'
  where execute' :: EE.ExecutableModule EE.MCJIT -> IO (Either Error Int64)
        execute' ee = do
          fn <- EE.getFunction ee (Name "main")
          case fn of
            (Just fn) -> do
              res <- runFn fn
              return $ Right res
            Nothing   -> return $ Left $ OtherError "Main function not found."

flattenError :: IO (Either String (Either Error a)) -> IO (Either Error a)
flattenError res = do
  res' <- res
  case res' of
    (Left msg) -> return $ Left $ OtherError msg
    (Right m)  -> return m

parseStage :: (Program -> IO ()) -> String -> String -> ErrorOrIO Program
parseStage report filename contents = stage report $ toErrorOrIO $ parse filename contents

toErrorOrIO :: ErrorOr Program -> ErrorOrIO Program
toErrorOrIO = mapExceptT $ return . runIdentity

stage :: (b -> IO ()) -> ErrorOrIO b -> ErrorOrIO b
stage report p = p >>= (\a -> lift (report a) >> return a)

checkStage :: (Program -> IO ()) -> Program -> ErrorOrIO Program
checkStage report program = stage report $ toErrorOrIO $ checkWithMain program

compile' :: (Context -> M.Module -> IO (Either Error a)) -> Reporter -> Module -> String -> String -> ErrorOrIO a
compile' moduleReporter reporter mod filename input = parseStage (reportParsed reporter) filename input
                                                      >>= checkStage (reportTyped reporter)
                                                      >>= codegenStage moduleReporter mod

compile :: Reporter -> String -> String -> ErrorOrIO ()
compile reporter filename input = ExceptT $ withModule filename $ \mod ->
  runExceptT $ compile' moduleReporter reporter mod filename input
  where moduleReporter :: Context -> M.Module -> IO (Either Error ())
        moduleReporter _ mod = reportModule reporter mod >> return (Right ())

run :: Reporter -> String -> String -> ErrorOrIO Int64
run reporter filename input = withModule filename $ \mod ->
  compile' runReporter reporter mod filename input
  where runReporter :: Context -> M.Module -> IO (Either Error Int64)
        runReporter context mod = reportModule reporter mod >> runStage context mod
