module Sara.TestUtils.TestUtils where

import Sara.Reporter

nopReporter :: Reporter
nopReporter = Reporter nop nop nop
  where nop :: a -> IO ()
        nop _ = return ()
