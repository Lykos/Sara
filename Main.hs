module Main where

import Parser

import Control.Monad.Trans

outputStrLn = putStrLn

process :: String -> IO ()
process line = do
  let res = parseToplevel "<stdin>" line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

main :: IO ()
main = getLine >>= process
