module TestUtils where

import Reporter

nopReporter :: Reporter
nopReporter = Reporter nop nop nop
  where nop :: a -> IO ()
        nop _ = return ()
