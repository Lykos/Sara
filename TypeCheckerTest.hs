{-# LANGUAGE TemplateHaskell #-}

module TypeCheckerTest (typeCheckerCheck) where

import Errors
import PrettyPrinter
import TestUtils
import TypeChecker
import Syntax

import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property

prop_addsTypes :: Program -> Property
prop_addsTypes p = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ render expected
                  ++ "\n\nActual:\n" ++ render actual
                  ++ "\n\nInput:\n" ++ prettyRender input
        input = clearTypes p
        expected = return p
        actual = typeCheck input
        render :: ErrorOr Program -> String
        render e = case runExcept e of
          Left e  -> show e
          Right r -> prettyRender r

return []

typeCheckerCheck = $quickCheckAll
