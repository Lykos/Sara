module Sara.Compiler (
  Reporter(..)
  , run
  , compile) where

import Sara.Parser
import Sara.TypeChecker
import Sara.PrettyPrinter
import Sara.Syntax
import Sara.CodeGenerator
import Sara.Errors
import Sara.Reporter

import Control.Monad.Except
import Control.Monad.Identity
import Data.Int
import Data.Bifunctor
import Foreign.Ptr ( FunPtr, castFunPtr )

import LLVM.General.PassManager
import LLVM.General.AST
import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Transforms
import LLVM.General.Analysis

import qualified LLVM.General.Module as M
import qualified LLVM.General.ExecutionEngine as EE

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

type ExceptOrIO a = ExceptT Error IO a

codegenStage :: (Context -> M.Module -> IO (Either Error a)) -> Module -> Program -> ExceptOrIO a
codegenStage report mod prog = ExceptT $ withContext $ \context -> codegen' context (codegen mod prog)
  where codegen' context mod = flattenError $ runExceptT $ M.withModuleFromAST context mod $ generate context
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

parseStage :: (Program -> IO ()) -> String -> String -> ExceptOrIO Program
parseStage report filename contents = stage report $ toError $ parse filename contents
  where toError :: ErrorOr Program -> ExceptOrIO Program
        toError e = case runExcept e of
          (Left err)  -> throwError err
          (Right res) -> return res

stage :: (b -> IO ()) -> ExceptOrIO b -> ExceptOrIO b
stage report = mapExceptT stage'
  where stage' e = do
          e' <- e
          case e' of
            Left err  -> return $ Left err
            Right res -> report res >> return (Right res)

checkStage :: (Program -> IO ()) -> Program -> ExceptOrIO Program
checkStage report program = stage report $ toError $ checkWithMain program
  where toError :: ErrorOr Program -> ExceptOrIO Program
        toError e = case runExcept e of
          (Left err)  -> throwError err
          (Right res) -> return res

compile' :: (Context -> M.Module -> IO (Either Error a)) -> Reporter -> Module -> String -> String -> ExceptOrIO a
compile' moduleReporter reporter mod filename input = parseStage (reportParsed reporter) filename input
                                                      >>= checkStage (reportTyped reporter)
                                                      >>= codegenStage moduleReporter mod

compile :: Reporter -> String -> String -> ExceptOrIO ()
compile reporter filename input = ExceptT $ withModule filename $ \mod ->
  runExceptT $ compile' moduleReporter reporter mod filename input
  where moduleReporter :: Context -> M.Module -> IO (Either Error ())
        moduleReporter context mod = reportModule reporter mod >> return (Right ())

run :: Reporter -> String -> String -> ExceptOrIO Int64
run reporter filename input = withModule filename $ \mod ->
  compile' runReporter reporter mod filename input
  where runReporter :: Context -> M.Module -> IO (Either Error Int64)
        runReporter context mod = reportModule reporter mod >> runStage context mod
