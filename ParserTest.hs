{-# LANGUAGE TemplateHaskell #-}

module ParserTest (parserCheck) where

import TestUtils
import Parser
import PrettyPrinter
import Syntax

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property
import Text.Parsec
import Data.Bifunctor

prop_prettyInv :: Program -> Property
prop_prettyInv xs = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ show expected
                  ++ "\nActual:\n" ++ show actual
                  ++ "\nInput:\n" ++ input
        untyped = clearTypes xs
        expected :: Either ParseError Program
        expected = Right untyped
        input = prettyRender untyped
        actual = second clearPositions $ Parser.parse testfile input

parserCheck = $quickCheckAll
