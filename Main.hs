module Main where

import Compiler
import PrettyPrinter
import Syntax
import Reporter
import Errors (showError)

import Control.Monad.Except
import System.IO
import System.Environment
import System.Console.Haskeline
import LLVM.General.Module

process :: String -> String -> IO ()
process fname input = do
  result <- runExceptT $ run reporter fname input
  case result of
    (Left err)  -> putStrLn $ showError input err
    (Right res) -> report "Result" $ show res

reporter :: Reporter
reporter = Reporter reportParsed reportTyped reportModule
           where reportParsed _    = return () -- reportProgram "Parsed Program"
                 reportTyped _     = return () -- reportProgram "Typed Program"
                 reportModule mod  = moduleLLVMAssembly mod >>= report "LLVM Code"
                 reportProgram :: String -> Program -> IO ()
                 reportProgram name program = report name $ prettyRender program

report :: String -> String -> IO ()
report name program = putStrLn $ "\n" ++ name ++ ":\n" ++ program

processFile :: String -> IO ()
processFile fname = readFile fname >>= process fname

main :: IO ()
main = do
  args <- getArgs
  case args of
    []   -> getContents >>= process "<stdin>"
    args -> mapM_ processFile args
