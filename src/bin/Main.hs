module Main where

import Sara.Compiler
import Sara.Reporter
import Sara.Errors (showError)

import Control.Monad.Except
import System.Environment
import LLVM.General.Module

process :: String -> String -> IO ()
process fname input = do
  result <- runExceptT $ run True reporter fname input
  case result of
    (Left err)  -> putStrLn $ showError input err
    (Right res) -> report "Result" $ show res

reporter :: Reporter
reporter = Reporter reportParsed reportTyped reportModule
           where reportParsed      = reportProgram "Parsed Program"
                 reportTyped       = reportProgram "Typed Program"
                 reportModule modl = moduleLLVMAssembly modl >>= report "LLVM Code"

processFile :: String -> IO ()
processFile fname = readFile fname >>= process fname

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= process "<stdin>"
    xs -> mapM_ processFile xs
