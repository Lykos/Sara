{-# LANGUAGE TemplateHaskell #-}

module RegressionTest (regressionCheck) where

import Errors
import PrettyPrinter
import AstTestUtils
import TestUtils
import Compiler
import Syntax
import TestUtils

import Data.Int
import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Test.QuickCheck.Monadic as M
import Test.QuickCheck.Property

isExpected :: Either Error Int64 -> Int64 -> Bool
isExpected (Left _)  _ = False
isExpected (Right n) m = n == m

checkRight :: String -> Int64 -> Property
checkRight input expected = M.monadicIO $ do
  actual <- M.run $ runExceptT $ run nopReporter testfile input
  let example = "\nExpected:\n" ++ show expected
                ++ "\n\nInput:\n" ++ input
                ++ "\n\nActual:\n" ++ show actual
  return $ example `counterexample` (liftBool $ isExpected actual expected)

prop_lol :: Bool
prop_lol = True

return []

regressionCheck = $quickCheckAll
