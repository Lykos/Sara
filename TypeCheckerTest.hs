{-# LANGUAGE TemplateHaskell #-}

module TypeCheckerTest (typeCheckerCheck) where

import Types
import Errors
import PrettyPrinter
import AstTestUtils
import TypeChecker
import Syntax

import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import Test.QuickCheck.Property
import Test.QuickCheck.Random

prop_addsTypes :: Program -> Property
prop_addsTypes p = check input expected actual
  where input = clearTypes p
        expected = return p
        actual = checkWithoutMain input

check :: Program -> ErrorOr Program -> ErrorOr Program -> Property
check input expected actual = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ render expected
                  ++ "\n\nActual:\n" ++ render actual
                  ++ "\n\nInput:\n" ++ prettyRender input

render :: ErrorOr Program -> String
render e = case runExcept e of
  Left e  -> show e
  Right r -> prettyRender r

-- This will only be used to generate variable names for the part that we need to complete a program, so using a fixed seed is fine.
seed :: Int
seed = 0

finish :: Gen Program -> Program
finish p = unGen p qcGen seed
  where qcGen :: QCGen
        qcGen = mkQCGen seed

prop_complainsReturnTypeMismatch :: Bool -> Type -> Expression -> Property
prop_complainsReturnTypeMismatch pure retTyp exp = retTyp /= expTyp ==> shrinking shrink input (\i -> check i (expected i) (actual i))
  where expTyp = typ exp
        complete = do
          name <- identifier
          let inferredSig = inferSignature pure name exp
          let wrongSig = inferredSig{ retType = retTyp }
          completeProgram [Function wrongSig exp pos]
        input = clearTypes $ finish complete
        actual input = checkWithoutMain input
        expected input = invalidRetType retTyp expTyp pos

return []

typeCheckerCheck = $quickCheckAll
