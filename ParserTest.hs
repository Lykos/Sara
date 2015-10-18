{-# LANGUAGE TemplateHaskell #-}

module ParserTest (parserCheck) where

import Errors
import TestUtils
import Parser
import PrettyPrinter
import Syntax

import Control.Monad.Except
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
        untyped :: Program
        untyped = clearTypes xs
        expected :: ErrorOr Program
        expected = return untyped
        input :: String
        input = prettyRender untyped
        actual :: ErrorOr Program
        actual = clearPositions' $ Parser.parse testfile input
        clearPositions' :: ErrorOr Program -> ErrorOr Program
        clearPositions' e = case runExcept e of
          Left err -> throwError err
          Right p  -> return $ clearPositions p

return []

parserCheck = $quickCheckAll
