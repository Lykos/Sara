module Main where

import Sara.ParserTest
import Sara.SyntaxTest
import Sara.TypeCheckerTest
import Sara.CompilerTest
import Sara.RegressionTest

import Test.Framework

main = defaultMain tests

tests = [ regressionGroup
        , syntaxGroup
        , typeCheckerGroup
        , compilerGroup
        , syntaxGroup ]
