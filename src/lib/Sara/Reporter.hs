module Sara.Reporter where

import Sara.Syntax
import LLVM.General.Module

data Reporter
  = Reporter { reportParsed :: Program -> IO ()
             , reportTyped :: Program -> IO ()
             , reportModule :: Module -> IO () }
