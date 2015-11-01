module Sara.OperatorsTest (operatorsGroup) where

import Sara.Operators
import Sara.AstTestUtils()

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

prop_unarySymbolInjective :: UnaryOperator -> UnaryOperator -> Property
prop_unarySymbolInjective a b = a /= b ==> unarySymbol a /= unarySymbol b

prop_binarySymbolInjective :: BinaryOperator -> BinaryOperator -> Property
prop_binarySymbolInjective a b = a /= b ==> binarySymbol a /= binarySymbol b

operatorsGroup :: Test
operatorsGroup = testGroup "OperatorsTests" [ testProperty "unary symbol injective" prop_unarySymbolInjective
                                            , testProperty "binary symbol injective" prop_binarySymbolInjective ]
