import ParserTest
import SyntaxTest
import TypeCheckerTest
import CompilerTest
import RegressionTest

main = do
  parserCheck
  regressionCheck
  compilerCheck
  syntaxCheck
  typeCheckerCheck
