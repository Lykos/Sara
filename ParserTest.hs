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
prop_prettyInv xs = example `counterexample` liftBool (equalPrograms actual expected)
  where example = "\nExpected:\n" ++ show expected
                  ++ "\nActual:\n" ++ show actual
                  ++ "\nInput:\n" ++ input
        untyped = clearTypes xs
        expected :: Either ParseError Program
        expected = Right untyped
        input = prettyRender untyped
        actual = second clearPositions $ Parser.parse testfile input

equalPrograms :: Either ParseError Program -> Either ParseError Program -> Bool
equalPrograms (Left _) _          = False
equalPrograms _ (Left _)          = False
equalPrograms (Right a) (Right b) = a == b

return []

parserCheck = $quickCheckAll
