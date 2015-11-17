module Sara.Parser.ParserTest (parserGroup) where

import Sara.Ast.Meta
import Sara.TestUtils.AstGenUtils()
import Sara.TestUtils.AstTestUtils
import qualified Sara.Parser.Parser as P
import Sara.Parser.PrettyPrinter

import Control.Monad.Except
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

prop_prettyInv :: TypeCheckerProgram -> Property
prop_prettyInv p = check untyped expected actual
  where untyped = clearTypes p
        expected = return untyped
        input = prettyRender untyped
        actual = clearPositions' $ P.parse testfile input
        clearPositions' e = case runExcept e of
          Left err -> throwError err
          Right p  -> return $ clearPositions p

parserGroup :: Test
parserGroup = testGroup "ParserTests" [ testProperty "pretty is the inverse of parse" prop_prettyInv ]
