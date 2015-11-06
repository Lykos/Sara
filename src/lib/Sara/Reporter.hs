module Sara.Reporter ( Reporter(..)
                     , reportProgram
                     , report ) where

import Sara.Meta
import Sara.Syntax
import Sara.PrettyPrinter
import LLVM.General.Module

data Reporter
  = Reporter { reportParsed :: ParserProgram -> IO ()
             , reportTyped :: SymbolizerProgram -> IO ()
             , reportModule :: Module -> IO () }

reportProgram :: String -> Program a b c d -> IO ()
reportProgram name program = report name $ prettyRender program

report :: String -> String -> IO ()
report name program = putStrLn $ "\n" ++ name ++ ":\n" ++ program
