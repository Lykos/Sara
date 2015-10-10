module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    ops = ["!", "~",
           "*", "/", "%",
           "+", "-",
           "<<", ">>",
           "<", "<=", ">", ">=",
           "==", "!=",
           "&", "^", "|",
           "&&", "^^", "||",
           "<==", "==>", "<==>", "<!=>",
           ";", ":"]
    names = ["function", "extern", "if", "then", "else", "true", "false", "Boolean", "Integer", "Double"]
    style = emptyDef {
               Token.commentStart = "/*"
	     , Token.commentEnd = "*/"
             , Token.commentLine = "//"
             , Token.nestedComments = True
             , Token.identStart = letter <|> char '_'
             , Token.identLetter = alphaNum <|> char '_'
             , Token.opStart = oneOf "<=>*/%&|;!~"
             , Token.opLetter = oneOf "=&|><"
             , Token.reservedOpNames = ops
             , Token.reservedNames = names
	     , Token.caseSensitive = True
             }

integerToken :: Parser Integer
integerToken = Token.integer lexer

doubleToken :: Parser Double
doubleToken = Token.float lexer

identifierToken :: Parser String
identifierToken = Token.identifier lexer

parensToken :: Parser a -> Parser a
parensToken = Token.parens lexer

semiSepToken :: Parser a -> Parser [a]
semiSepToken = Token.semiSep lexer

commaSepToken :: Parser a -> Parser [a]
commaSepToken = Token.commaSep lexer

reservedToken :: String -> Parser ()
reservedToken = Token.reserved lexer

reservedOpToken :: String -> Parser ()
reservedOpToken = Token.reservedOp lexer
