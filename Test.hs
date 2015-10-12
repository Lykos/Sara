module Test where

import ParserTest
import SyntaxTest
import TypeCheckerTest

main = do
  parserCheck
  syntaxCheck
  typeCheckerCheck
