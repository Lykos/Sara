import ParserTest
import SyntaxTest
import TypeCheckerTest
import CompilerTest
import RegressionTest

main = do
  regressionCheck
  compilerCheck
  parserCheck
  syntaxCheck
  typeCheckerCheck
