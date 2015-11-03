module Sara.TypeCheckerTest (typeCheckerGroup) where

import Sara.Types
import qualified Sara.Errors as E
import Sara.PrettyPrinter
import Sara.AstGenUtils
import Sara.TypeChecker
import qualified Sara.Syntax as S
import Sara.Meta

import Control.Monad.Except
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Property
import Test.QuickCheck.Random

prop_addsTypes :: TypeCheckerProgram -> Property
prop_addsTypes p = check input expected actual
  where input = clearTypes p
        expected = return p
        actual = liftM clearSymbols $ checkWithoutMain input

check :: ParserProgram -> E.ErrorOr TypeCheckerProgram -> E.ErrorOr TypeCheckerProgram -> Property
check input expected actual = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ render expected
                  ++ "\n\nActual:\n" ++ render actual
                  ++ "\n\nInput:\n" ++ prettyRender input

render :: E.ErrorOr TypeCheckerProgram -> String
render e = case runExcept e of
  Left e  -> show e
  Right r -> prettyRender r

-- This will only be used to generate variable names for the part that we need to complete a program, so using a fixed seed is fine.
seed :: Int
seed = 0

finish :: Gen TypeCheckerProgram -> TypeCheckerProgram
finish p = unGen p qcGen seed
  where qcGen :: QCGen
        qcGen = mkQCGen seed

complainsReturnTypeMismatch :: Bool -> Type -> TypeCheckerExpression -> Property
complainsReturnTypeMismatch pure retTyp exp = retTyp /= expTyp ==> shrinking shrink input (\inp -> check (clearTypes inp) expected (actual inp))
  where expTyp = expressionTyp exp
        complete = do
          name <- identifier
          let inferredSig = inferSignature pure name [] [] exp
          let wrongSig = inferredSig{ S.retType = retTyp }
          completeProgram [S.Function wrongSig exp mkNodeMeta]
        input = finish complete
        actual = liftM clearSymbols . checkWithoutMain . clearTypes
        expected = E.invalidRetType retTyp expTyp pos

prop_complainsPureReturnTypeMismatch :: Type -> PureExpression -> Property
prop_complainsPureReturnTypeMismatch typ exp = complainsReturnTypeMismatch True typ $ runPureExpression exp

prop_complainsImpureReturnTypeMismatch :: Type -> TypeCheckerExpression -> Property
prop_complainsImpureReturnTypeMismatch = complainsReturnTypeMismatch False

typeCheckerGroup :: Test
typeCheckerGroup = testGroup "TypeCheckerTests" [ testProperty "adds types" prop_addsTypes
                                                , testProperty "complains about pure return type mismatches" prop_complainsPureReturnTypeMismatch
                                                , testProperty "complains about impure return type mismatches" prop_complainsImpureReturnTypeMismatch ]
