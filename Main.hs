module Main where

import Parser
import TypeChecker

import Control.Monad.Trans

outputStrLn = putStrLn

process :: String -> IO ()
process line = do
  let parsed = parseToplevel "<stdin>" line
  case parsed of
    Left err -> print err
    Right asts -> do
      mapM_ print asts
      let typed = typeCheck asts
      case typed of
        Error err        -> print err
        Result typedAsts -> mapM_ print typedAsts

main :: IO ()
main = getLine >>= process
