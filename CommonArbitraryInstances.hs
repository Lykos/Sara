module CommonArbitraryInstances where

import Syntax
import Test.QuickCheck

instance Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators

