module Reporter where

import Syntax
import Errors
import Data.Int
import LLVM.General.Module

data Reporter
  = Reporter { reportParsed :: Program -> IO ()
             , reportTyped :: Program -> IO ()
             , reportModule :: Module -> IO () }
