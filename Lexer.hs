module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    ops = ["+", "*", "-", "/", "%", "&&", "||", ";", "==", "<=>", "!=", ">=", "<=", "="]
    names = ["function", "extern"]
    style = emptyDef {
               Token.commentStart = "/*"
	     , Token.commentEnd = "*/"
             , Token.commentLine = "//"
             , Token.nestedComments = True
             , Token.identStart = letter <|> char '_'
             , Token.identLetter = alphaNum <|> char '_'
             , Token.opStart = oneOf "<=>*/%&|;!"
             , Token.opLetter = oneOf "=&|>"
             , Token.reservedOpNames = ops
             , Token.reservedNames = names
	     , Token.caseSensitive = True
             }

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

