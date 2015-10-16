module Main where

import Parser
import TypeChecker
import PrettyPrinter
import CodeGenerator

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import LLVM.General.AST

initModule :: Module
initModule = emptyModule "my cool jit"

process :: Module -> String -> IO (Maybe Module)
process modo contents = do
  let parsed = parse "<stdin>" contents
  case parsed of
    Left err -> print err >> return Nothing
    Right asts -> do
      putStrLn "\nParsed AST:"
      print asts
      putStrLn "\nPretty AST:"
      putStrLn $ prettyRender asts
      let typed = typeCheck asts
      case typed of
        Error err        -> print err >> return Nothing
        Result typedAsts -> do
          putStrLn "\nTyped AST:"
          putStrLn $ prettyRender typedAsts
          ast <- codegen modo typedAsts
          return $ Just ast

processFile :: String -> IO (Maybe Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
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
