module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

type DeclarationOrExpression = Either Declaration Expression

declarationOrExpression :: Parser DeclarationOrExpression
declarationOrExpression = try topLevelDeclaration
                          <|> toplevelExpression
                          <?> "declaration or expression"

topLevelDeclaration :: Parser DeclarationOrExpression
topLevelDeclaration = do
  decl <- declaration
  return $ Left decl

declaration :: Parser Declaration
declaration = try function
              <|> try extern
              <?> "declaration"

function :: Parser Declaration
function = do
  reservedToken "function"
  name <- identifierToken
  args <- parensToken $ many variable
  body <- expression
  return $ Function name args body

extern :: Parser Declaration
extern = do
  reservedToken "extern"
  name <- identifierToken
  args <- parensToken $ many variable
  return $ Extern name args

toplevelExpression :: Parser DeclarationOrExpression
toplevelExpression = do
  expr <- expression
  return $ Right expr

expression :: Parser Expression
expression = Expr.buildExpressionParser operatorTable term

operatorTable = [[unaryOperator "+" UnaryPlus,
                  unaryOperator "-" UnaryMinus,
                  unaryOperator "~" BitwiseNot,
                  unaryOperator "!" LogicalNot]
                 ,[binaryOperator "*" Times Expr.AssocLeft,
                  binaryOperator "%" Modulo Expr.AssocLeft,
                  binaryOperator "/" Divide Expr.AssocLeft]
                ,[binaryOperator "+" Plus Expr.AssocLeft,
                  binaryOperator "-" Minus Expr.AssocLeft]
                ,[binaryOperator "<<" LeftShift Expr.AssocLeft,
                  binaryOperator ">>" RightShift Expr.AssocLeft]
                ,[binaryOperator "<" LessThan Expr.AssocLeft,
                  binaryOperator "<=" AtMost Expr.AssocLeft,
                  binaryOperator ">" GreaterThan Expr.AssocLeft,
                  binaryOperator ">=" AtLeast Expr.AssocLeft]
                ,[binaryOperator "==" EqualTo Expr.AssocLeft,
                  binaryOperator "!=" NotEqualTo Expr.AssocLeft]
                ,[binaryOperator "&" BitwiseAnd Expr.AssocLeft]
                ,[binaryOperator "^" BitwiseXor Expr.AssocLeft]
                ,[binaryOperator "|" BitwiseOr Expr.AssocLeft]
                ,[binaryOperator "&&" LogicalAnd Expr.AssocLeft]
                ,[binaryOperator "^^" LogicalXor Expr.AssocLeft]
                ,[binaryOperator "||" LogicalOr Expr.AssocLeft]
                ,[binaryOperator "<==" ImpliedBy Expr.AssocLeft,
                  binaryOperator "==>" Implies Expr.AssocRight]
                ,[binaryOperator "<==>" EquivalentTo Expr.AssocLeft,
                  binaryOperator "<!=>" NotEquivalentTo Expr.AssocLeft]]

unaryOperator symbol operator = Expr.Prefix (reservedOpToken symbol >> return (UnaryOperation operator))
binaryOperator symbol operator assoc = Expr.Infix (reservedOpToken symbol >> return (BinaryOperation operator)) assoc

term :: Parser Expression
term = try integer
       <|> try call
       <|> try conditional
       <|> variable
       <|> parensToken expression
       <?> "expression"
      
integer :: Parser Expression
integer = do
  n <- integerToken
  return $ Integer n

variable :: Parser Expression
variable = do
  var <- identifierToken
  return $ Variable var

call :: Parser Expression
call = do
  name <- identifierToken
  args <- parensToken $ many expression
  return $ Call name args

conditional :: Parser Expression
conditional = do
  reservedToken "if"
  cond <- expression
  reservedToken "then"
  ifExpr <- expression
  reservedToken "else"
  thenExpr <- expression
  return $ Conditional cond ifExpr thenExpr

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [DeclarationOrExpression]
toplevel = many $ do
    def <- declarationOrExpression
    reservedOpToken ";"
    return def

parseExpr :: String -> Either ParseError Expression
parseExpr s = parse (contents expression) "<stdin>" s

parseToplevel :: String -> Either ParseError [DeclarationOrExpression]
parseToplevel s = parse (contents toplevel) "<stdin>" s
