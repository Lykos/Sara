{-# LANGUAGE TemplateHaskell #-}

module TypeCheckerTest (typeCheckerCheck) where

import PrettyPrinter
import TestUtils
import TypeChecker
import Syntax

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property

prop_addsTypes :: Program -> Property
prop_addsTypes p = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ render expected
                  ++ "\n\nActual:\n" ++ render actual
                  ++ "\n\nInput:\n" ++ prettyRender input
        input = clearTypes p
        expected = Result p
        actual = typeCheck input
        render :: TypeErrorOr Program -> String
        render (Error e)  = show e
        render (Result r) = prettyRender r

typeCheckerCheck = $quickCheckAll
