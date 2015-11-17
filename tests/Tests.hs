module Main where

import Sara.Parser.ParserTest
import Sara.Ast.OperatorsTest
import Sara.Semantic.TypeCheckerTest
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
