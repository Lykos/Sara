module Sara.ParserTest (parserGroup) where

import Sara.Meta
import Sara.Errors
import Sara.AstTestUtils
import qualified Sara.Parser as P
import Sara.PrettyPrinter

import Control.Monad.Except
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Property

prop_prettyInv :: TypeCheckerProgram -> Property
prop_prettyInv xs = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ show expected
                  ++ "\nActual:\n" ++ show actual
                  ++ "\nInput:\n" ++ input
        untyped :: ParserProgram
        untyped = clearTypes xs
        expected :: ErrorOr ParserProgram
        expected = return untyped
        input :: String
        input = prettyRender untyped
        actual :: ErrorOr ParserProgram
        actual = clearPositions' $ P.parse testfile input
        clearPositions' :: ErrorOr ParserProgram -> ErrorOr ParserProgram
        clearPositions' e = case runExcept e of
          Left err -> throwError err
          Right p  -> return $ clearPositions p

parserGroup :: Test
parserGroup = testGroup "ParserTests" [ testProperty "pretty is the inverse of parse" prop_prettyInv ]
