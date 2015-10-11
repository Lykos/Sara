module Main where

import Parser
import TypeChecker
import PrettyPrinter

import Control.Monad.Trans

outputStrLn = putStrLn

process :: String -> IO ()
process line = do
  let parsed = parse "<stdin>" line
  case parsed of
    Left err -> print err
    Right asts -> do
      putStrLn "\nParsed AST:"
      print asts
      putStrLn "\nPretty AST:"
      putStrLn $ prettyRender asts
      let typed = typeCheck asts
      case typed of
        Error err        -> print err
        Result typedAsts -> do
          putStrLn "\nTyped AST:"
          putStrLn $ prettyRender typedAsts

main :: IO ()
main = getLine >>= process
