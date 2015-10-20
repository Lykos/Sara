{-# LANGUAGE TemplateHaskell #-}

module CompilerTest (compilerCheck) where

import Errors
import PrettyPrinter
import AstTestUtils
import Compiler
import Syntax
import TestUtils

import Data.Either
import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.All
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

return []

compilerCheck = $quickCheckAll
