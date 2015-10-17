module Parser (Parser.parse) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Void

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax
import Types
import Operators
import AstUtils

declarationAst :: Parser DeclarationAst
declarationAst = do
  decl <- declaration
  addPosition . return $ DeclarationAst decl

declaration :: Parser Declaration
declaration = try function
              <|> try extern
              <|> try method
              <?> "declaration"

functionOrMethod :: String -> FunctionOrMethodConstructor -> Parser Declaration
functionOrMethod keyword constructor = do
  reservedToken keyword
  sig <- Parser.signature
  reservedOpToken "="
  body <- expressionAst
  return $ constructor sig body

function :: Parser Declaration
function = functionOrMethod "function" Function

method :: Parser Declaration
method = functionOrMethod "method" Method

extern :: Parser Declaration
extern = do
  reservedToken "extern"
  sig <- Parser.signature
  return $ Extern sig

signature :: Parser Signature
signature = do
  name <- identifierToken
  args <- parensToken $ commaSep typedVariable
  reservedToken ":"
  retType <- typeExpression
  return $ Signature name args retType

typedVariable :: Parser TypedVariable
typedVariable = do
  name <- identifierToken
  reservedToken ":"
  varType <- typeExpression
  addPosition . return $ TypedVariable name varType

typeExpression :: Parser Type
typeExpression = try unitType
                 <|> try booleanType
                 <|> try integerType
                 <|> try doubleType
                 <?> "type"

unitType :: Parser Type
unitType = reservedToken "Unit" >> return Types.Unit

booleanType :: Parser Type
booleanType = reservedToken "Boolean" >> return Types.Boolean

integerType :: Parser Type
integerType = reservedToken "Integer" >> return Types.Integer

doubleType :: Parser Type
doubleType = reservedToken "Double" >> return Types.Double

expressionAst :: Parser ExpressionAst
expressionAst = Expr.buildExpressionParser operatorTable term

operatorTable = [ [ unaryOperator UnaryPlus
                  , unaryOperator UnaryMinus
                  , unaryOperator BitwiseNot
                  , unaryOperator LogicalNot]
                 , [ binaryOperator Times Expr.AssocLeft
                   , binaryOperator Modulo Expr.AssocLeft
                   , binaryOperator DividedBy Expr.AssocLeft]
                , [ binaryOperator Plus Expr.AssocLeft
                  , binaryOperator Minus Expr.AssocLeft]
                , [ binaryOperator LeftShift Expr.AssocLeft
                  , binaryOperator RightShift Expr.AssocLeft]
                , [ binaryOperator LessThan Expr.AssocLeft
                  , binaryOperator AtMost Expr.AssocLeft
                  , binaryOperator GreaterThan Expr.AssocLeft
                  , binaryOperator AtLeast Expr.AssocLeft]
                , [ binaryOperator EqualTo Expr.AssocLeft
                  , binaryOperator NotEqualTo Expr.AssocLeft]
                , [ binaryOperator BitwiseAnd Expr.AssocLeft]
                , [ binaryOperator BitwiseXor Expr.AssocLeft]
                , [ binaryOperator BitwiseOr Expr.AssocLeft]
                , [ binaryOperator LogicalAnd Expr.AssocLeft]
                , [ binaryOperator LogicalXor Expr.AssocLeft]
                , [ binaryOperator LogicalOr Expr.AssocLeft]
                , [ binaryOperator ImpliedBy Expr.AssocLeft
                  , binaryOperator Implies Expr.AssocRight]
                , [ binaryOperator EquivalentTo Expr.AssocLeft
                  , binaryOperator NotEquivalentTo Expr.AssocLeft]]

unaryOperator operator = Expr.Prefix (operation (unarySymbol operator) (unaryOperation operator))

binaryOperator operator assoc = Expr.Infix (operation (binarySymbol operator) (binaryOperation operator)) assoc

operation :: String -> (SourcePos -> a) -> Parser a
operation symbol op = do
  reservedOpToken symbol
  addPosition . return $ op

unaryOperation :: UnaryOperator -> SourcePos -> ExpressionAst -> ExpressionAst
unaryOperation op pos exp = ExpressionAst (UnaryOperation op exp) Unknown pos

binaryOperation :: BinaryOperator -> SourcePos -> ExpressionAst -> ExpressionAst -> ExpressionAst
binaryOperation op pos left right = ExpressionAst (BinaryOperation op left right) Unknown pos

term :: Parser ExpressionAst
term = do
  simpleExpressionAst
  <|> parensToken expressionAst
  <?> "expression"

addPosition :: Parser (SourcePos -> a) -> Parser a
addPosition parser = do
  pos <- getPosition
  ast <- parser
  return $ ast pos

simpleExpressionAst :: Parser ExpressionAst
simpleExpressionAst = do
  t <- expression
  addPosition . return $ ExpressionAst t Unknown

expression :: Parser Expression
expression = try unit
             <|> try boolean
             <|> try double
             <|> try integer
             <|> try call
             <|> try conditional
             <|> try block
             <|> variable

empty :: Parser Void
empty = return undefined

unit :: Parser Expression
unit = parensToken empty >> return Syntax.Unit

boolean :: Parser Expression
boolean = (reservedToken "true" >> return (Syntax.Boolean True))
          <|> (reservedToken "false" >> return (Syntax.Boolean False))

integer :: Parser Expression
integer = do
  n <- integerToken
  return $ Syntax.Integer n

double :: Parser Expression
double = do
  n <- doubleToken
  return $ Syntax.Double n

variable :: Parser Expression
variable = do
  var <- identifierToken
  return $ Variable var

call :: Parser Expression
call = do
  name <- identifierToken
  args <- parensToken $ commaSep expressionAst
  return $ Call name args

conditional :: Parser Expression
conditional = do
  reservedToken "if"
  cond <- expressionAst
  reservedToken "then"
  ifExpr <- expressionAst
  reservedToken "else"
  thenExpr <- expressionAst
  return $ Conditional cond ifExpr thenExpr

block :: Parser Expression
block = do
  pos <- getPosition
  exps <- bracesToken $ semiSep expressionAst
  return $ if (null exps) then
             Block [] (ExpressionAst Syntax.Unit Unknown pos)
           else
             Block (init exps) (last exps)

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser Program
toplevel = do
  program <- semiSep declarationAst
  return $ Program program

parse :: String -> String -> Either ParseError Program
parse source s = Text.Parsec.parse (contents toplevel) source s
