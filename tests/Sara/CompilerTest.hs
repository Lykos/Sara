module Sara.CompilerTest (compilerGroup) where

import Sara.PrettyPrinter
import Sara.AstTestUtils
import Sara.Compiler
import Sara.Syntax
import Sara.TestUtils

import Data.Either
import Control.Monad.Except
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as M
import Test.QuickCheck.Property

prop_generatesCode :: Program -> Property
prop_generatesCode p = checkRight input
  where input :: String
        input = prettyRender untyped
        untyped :: Program
        untyped = clearTypes p
        checkRight :: String -> Property
        checkRight input = M.monadicIO $ do
          actual <- M.run $ runExceptT $ compile nopReporter testfile input
          let example = "\nInput:\n" ++ input ++ "\n\nActual:\n" ++ show actual
          return $ example `counterexample` liftBool (isRight actual)

compilerGroup :: Test
compilerGroup = testGroup "Compiler Tests" [ testProperty "generates code" prop_generatesCode ]
