module Main where

import Compiler
import PrettyPrinter
import Syntax
import Reporter

import System.IO
import System.Environment
import System.Console.Haskeline
import LLVM.General.Module

process :: String -> String -> IO ()
process = run reporter

reporter :: Reporter
reporter = Reporter reportParsed reportTyped reportModule reportResult reportError
           where reportParsed      = reportProgram "Parsed Program"
                 reportTyped       = reportProgram "Typed Program"
                 reportModule mod  = moduleLLVMAssembly mod >>= report "LLVM Code"
                 reportResult n    = report "Result" $ show n
                 reportError error = print error
                 report :: String -> String -> IO ()
                 report name program = putStrLn $ "\n" ++ name ++ ":\n" ++ program
                 reportProgram :: String -> Program -> IO ()
                 reportProgram name program = report name $ prettyRender program

processFile :: String -> IO ()
processFile fname = readFile fname >>= process fname

main :: IO ()
main = do
  args <- getArgs
  case args of
    []   -> getContents >>= process "<stdin>"
    args -> sequence (map processFile args) >> return ()
