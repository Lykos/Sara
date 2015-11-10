module Sara.Lexer where

import Sara.Operators
import Sara.Types
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as T

reservedOpNames :: [String]
reservedOpNames = map unarySymbol unaryOperators
                  ++ map binarySymbol binaryOperators
                  ++ [";", ":"]

reservedNames :: [String]
reservedNames = [ "function", "extern", "method", "if", "then"
                , "else", "while", "true", "false", "requires"
                , "ensures", "assert", "assume", "assertAndCollapse"
                , "invariant"] ++ map show types

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where
    style = emptyDef {
               T.commentStart = "/*"
               , T.commentEnd = "*/"
               , T.commentLine = "//"
               , T.nestedComments = True
               , T.identStart = letter <|> char '_'
               , T.identLetter = alphaNum <|> char '_'
               , T.reservedOpNames = reservedOpNames
               , T.reservedNames = reservedNames
               , T.caseSensitive = True }

integerToken :: Parser Integer
integerToken = T.integer lexer

doubleToken :: Parser Double
doubleToken = T.float lexer

identifierToken :: Parser String
identifierToken = T.identifier lexer

parensToken :: Parser a -> Parser a
parensToken = T.parens lexer

bracesToken :: Parser a -> Parser a
bracesToken = T.braces lexer

semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep lexer

semi :: Parser String
semi = T.semi lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

reservedToken :: String -> Parser ()
reservedToken = T.reserved lexer

reservedOpToken :: String -> Parser ()
reservedOpToken = T.reservedOp lexer

whiteSpaceToken :: Parser ()
whiteSpaceToken = T.whiteSpace lexer

symbol :: String -> Parser String
symbol = T.symbol lexer
