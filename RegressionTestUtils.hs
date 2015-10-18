module RegressionTestUtils (
  Expectation
  , parseExpectation) where

import Errors
import Data.Int
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

-- The input files can have returns directives in a comment at the top to indicate what the file is supposed to return.
-- // returns 5

type Expectation = Int64

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef { Token.reservedNames = ["//", "returns"] }

integerToken :: Parser Integer
integerToken = Token.integer lexer

whiteSpaceToken :: Parser ()
whiteSpaceToken = Token.whiteSpace lexer

reservedToken :: String -> Parser ()
reservedToken = Token.reserved lexer

expectation :: Parser Expectation
expectation = do
  whiteSpaceToken
  reservedToken "//"
  reservedToken "returns"
  n <- integerToken
  return $ fromInteger n

parseExpectation :: String -> String -> Expectation
parseExpectation fname input = case Text.Parsec.parse expectation fname input of
  (Left err) -> error $ show err
  (Right p)  -> p
