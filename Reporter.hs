module Reporter where

import Syntax
import Errors
import Data.Int
import LLVM.General.Module

data Reporter
  = Reporter { reportParsed :: Program -> IO ()
             , reportTyped :: Program -> IO ()
             , reportModule :: Module -> IO ()
             , reportResult :: Int64 -> IO ()
             , reportError :: Error -> IO () }
