import ParserTest
import SyntaxTest
import TypeCheckerTest
import CompilerTest
import RegressionTest

main = do
  regressionCheck
  typeCheckerCheck
  parserCheck
  compilerCheck
  syntaxCheck
