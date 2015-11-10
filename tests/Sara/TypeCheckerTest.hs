module Sara.TypeCheckerTest (typeCheckerGroup) where

import Control.Monad.Except
import Sara.AstGenUtils()
import Sara.AstTestUtils
import Sara.TypeChecker
import Sara.Meta

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

prop_addsTypes :: TypeCheckerProgram -> Property
prop_addsTypes p = check input expected actual
  where input = clearTypes p
        expected = return p
        actual = liftM clearSymbols $ liftM clearPureness $ checkWithoutMain input

typeCheckerGroup :: Test
typeCheckerGroup = testGroup "TypeCheckerTests" [ testProperty "adds types" prop_addsTypes ]
