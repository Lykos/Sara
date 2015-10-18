module Compiler (
  Reporter(..)
  , run
  , compile) where

import Parser
import TypeChecker
import PrettyPrinter
import CodeGenerator
import Syntax
import CodeGenerator
import Errors

import Control.Monad.Except
import Control.Monad.Identity
import Data.Int
import Data.Bifunctor
import Foreign.Ptr ( FunPtr, castFunPtr )

import LLVM.General.Context
import LLVM.General.PassManager
import LLVM.General.AST
import LLVM.General.Context
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

codegenStage :: Module -> Program -> (Context -> M.Module -> IO ()) -> ExceptOrIO Module
codegenStage mod prog report = withExceptT OtherError $ ExceptT $ withContext $ \context -> runExceptT $ M.withModuleFromAST context mod' $ generate context
  where generate :: Context -> M.Module -> IO Module
        generate context m = withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          report context m
          -- Return the optimized module
          M.moduleAST m
        mod' = codegen mod prog

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int64) -> (IO Int64)

runFn :: FunPtr a -> IO Int64
runFn fn = haskFun (castFunPtr fn :: FunPtr (IO Int64))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

runStage :: Context -> M.Module -> (Int64 -> IO ()) -> IO ()
runStage context mod report = jit context $ \engine -> EE.withModuleInEngine engine mod execute'
  where execute' :: EE.ExecutableModule EE.MCJIT -> IO ()
        execute' ee = do
          fn <- EE.getFunction ee (Name "main")
          case fn of
            (Just fn) -> do
              res <- runFn fn
              report res
            Nothing   -> return ()

flattenError :: IO (Either String (Either Error a)) -> IO (Either Error a)
flattenError res = do
  res' <- res
  case res' of
    (Left msg) -> return $ Left $ OtherError msg
    (Right m)  -> return m

parseStage :: String -> String -> (Program -> IO ()) -> ExceptOrIO Program
parseStage filename contents = stage $ toError $ parse filename contents
  where toError :: ErrorOr Program -> ExceptOrIO Program
        toError e = case runExcept e of
          (Left err)  -> throwError err
          (Right res) -> return res

stage :: ExceptOrIO b -> (b -> IO ()) -> ExceptOrIO b
stage e report = mapExceptT stage' e
  where stage' e = do
          e' <- e
          case e' of
            Left err  -> return $ Left err
            Right res -> report res >> return (Right res)

typeCheckStage :: Program -> (Program -> IO ()) -> ExceptOrIO Program
typeCheckStage program = stage $ toError $ typeCheckWithMain program
  where toError :: ErrorOr Program -> ExceptOrIO Program
        toError e = case runExcept e of
          (Left err)  -> throwError err
          (Right res) -> return res

data Reporter
  = Reporter { reportParsed :: Program -> IO ()
             , reportTyped :: Program -> IO ()
             , reportModule :: M.Module -> IO ()
             , reportResult :: Int64 -> IO ()
             , reportError :: Error -> IO () }

compile'' :: (Context -> M.Module -> IO ()) -> Reporter -> Module -> String -> String -> ExceptOrIO Module
compile'' moduleReporter reporter mod filename input = parseStage filename input (reportParsed reporter)
                                                      >>= flip typeCheckStage (reportTyped reporter)
                                                      >>= flip (codegenStage mod) moduleReporter

compile' :: Reporter -> String -> String -> ExceptOrIO Module
compile' reporter filename input = withModule filename $ \mod ->
  compile'' (\c m -> reportModule reporter m) reporter mod filename input

compile :: Reporter -> String -> String -> IO ()
compile reporter filename input = handleError reporter $ compile' reporter filename input

handleError :: Reporter -> ExceptOrIO Module -> IO ()
handleError reporter res = do
  res' <- runExceptT res
  case res' of
    Left error -> reportError reporter error
    Right mod  -> return ()

run :: Reporter -> String -> String -> IO ()
run reporter filename input = handleError reporter $ run' reporter filename input

run' :: Reporter -> String -> String -> ExceptOrIO Module
run' reporter filename input = withModule filename $ \mod ->
  compile'' runReporter reporter mod filename input
  where runReporter :: Context -> M.Module -> IO ()
        runReporter context mod = do
          reportModule reporter mod
          runStage context mod $ reportResult reporter
