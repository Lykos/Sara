module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

parser :: Parser Expression
parser = Expr.buildExpressionParser operatorTable expression

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

expressionOrDeclaration :: Parser Expression
expressionOrDeclaration = try function
                          <|> try extern
                          <|> expression
                          <?> "expression or declaration"

expression :: Parser Expression
expression = try integer
             <|> try call
             <|> try conditional
             <|> variable
             <|> parensToken expression
             <?> "expression"

function :: Parser Expression
function = do
  reservedToken "function"
  name <- identifierToken
  args <- parensToken $ many variable
  body <- expression
  return $ Function name args body

extern :: Parser Expression
extern = do
  reservedToken "extern"
  name <- identifierToken
  args <- parensToken $ many variable
  return $ Extern name args
      
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
