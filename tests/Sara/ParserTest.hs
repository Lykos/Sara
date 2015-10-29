module Sara.ParserTest (parserGroup) where

import Sara.Errors
import Sara.AstTestUtils
import qualified Sara.Parser as P
import Sara.PrettyPrinter
import Sara.Syntax

import Control.Monad.Except
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Property

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
        actual = clearPositions' $ P.parse testfile input
        clearPositions' :: ErrorOr Program -> ErrorOr Program
        clearPositions' e = case runExcept e of
          Left err -> throwError err
          Right p  -> return $ clearPositions p

parserGroup :: Test
parserGroup = testGroup "Parser Tests" [ testProperty "pretty is the inverse of parse" prop_prettyInv ]
