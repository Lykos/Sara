module Main where

import Sara.ParserTest
import Sara.OperatorsTest
import Sara.TypeCheckerTest
import Sara.CompilerTest
import Sara.RegressionTest

import Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ regressionGroup
        , typeCheckerGroup
        , parserGroup
        , compilerGroup
        , operatorsGroup ]
