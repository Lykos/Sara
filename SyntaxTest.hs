{-# LANGUAGE TemplateHaskell #-}

module SyntaxTest (syntaxCheck) where

import Syntax
import TestUtils
import Test.QuickCheck
import Test.QuickCheck.All

prop_unarySymbolInjective a b = a /= b ==> unarySymbol a /= unarySymbol b

prop_binarySymbolInjective a b = a /= b ==> binarySymbol a /= binarySymbol b

syntaxCheck = $quickCheckAll
