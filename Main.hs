module Main where

import Parser
import TypeChecker
import PrettyPrinter
import CodeGenerator
import Syntax

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except

import System.IO
import System.Environment
import System.Console.Haskeline
import Foreign.Ptr ( FunPtr, castFunPtr )
import Data.Int

import LLVM.General.PassManager
import LLVM.General.AST
import LLVM.General.Context
import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

import qualified LLVM.General.ExecutionEngine as EE
import qualified LLVM.General.Module as M

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int64) -> (IO Int64)

(>>>=) :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
a >>>= f = a >>= liftMaybe f
  where liftMaybe :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
        liftMaybe _ Nothing  = return Nothing
        liftMaybe f (Just b) = f b

initModule :: Module
initModule = emptyModule "my cool jit"

run :: FunPtr a -> IO Int64
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int64))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

runStage' :: Module -> Program -> IO (Either String Module)
runStage' mod prog = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ M.withModuleFromAST context mod' $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          s <- M.moduleLLVMAssembly m
          putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()

          -- Return the optimized module
          M.moduleAST m
  where mod' = codegen mod prog

runStage :: Module -> Program -> IO (Maybe Module)
runStage m p = runStage' m p >>= eitherToMaybe
  where eitherToMaybe :: Either String Module -> IO (Maybe Module)
        eitherToMaybe (Left err) = print err >> return Nothing
        eitherToMaybe (Right m)  = return $ Just m


parseStage :: String -> IO (Maybe Program)
parseStage contents = do
  let parsed = parse "<stdin>" contents
  case parsed of
    Left err -> print err >> return Nothing
    Right program -> do
      putStrLn "\nParsed AST:\n"
      putStrLn $ prettyRender program
      return $ Just program

typeCheckStage :: Program -> IO (Maybe Program)
typeCheckStage program = do
  let typed = typeCheckWithMain program
  case typed of
    Error err        -> print err >> return Nothing
    Result typedAsts -> do
      putStrLn "\nTyped AST:"
      putStrLn $ prettyRender typedAsts
      return $ Just typedAsts

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

process :: Module -> String -> IO (Maybe Module)
process mod input = parseStage input >>>= typeCheckStage >>>= runStage mod

processFile :: String -> IO (Maybe Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where loop mod = do
          minput <- getInputLine "ready> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
              modn <- liftIO $ process mod input
              case modn of
                Just modn -> loop modn
                Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
