module Sara.SyntaxTest (syntaxGroup) where

import Sara.Syntax
import Sara.AstTestUtils
import Sara.Operators

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

prop_unarySymbolInjective a b = a /= b ==> unarySymbol a /= unarySymbol b

prop_binarySymbolInjective a b = a /= b ==> binarySymbol a /= binarySymbol b

syntaxGroup = testGroup "Syntax Tests" [ testProperty "unary symbol injective" prop_unarySymbolInjective
                                       , testProperty "binary symbol injective" prop_binarySymbolInjective ]
