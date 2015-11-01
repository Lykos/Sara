module Sara.Reporter where

import Sara.Meta
import LLVM.General.Module

data Reporter
  = Reporter { reportParsed :: ParserProgram -> IO ()
             , reportTyped :: SymbolizerProgram -> IO ()
             , reportModule :: Module -> IO () }
