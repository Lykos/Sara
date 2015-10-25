module Sara.Reporter where

import Sara.Syntax
import Sara.Errors
import Data.Int
import LLVM.General.Module

data Reporter
  = Reporter { reportParsed :: Program -> IO ()
             , reportTyped :: Program -> IO ()
             , reportModule :: Module -> IO () }
