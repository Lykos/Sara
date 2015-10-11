module Parser (parseToplevel) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax
import Types

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
  sig <- Parser.signature
  body <- expressionAst
  return $ Function sig body

extern :: Parser Declaration
extern = do
  reservedToken "extern"
  sig <- Parser.signature
  return $ Extern sig

signature :: Parser Signature
signature = do
  name <- identifierToken
  args <- parensToken $ many typedVariable
  reservedToken ":"
  retType <- typeExpression
  return $ Signature name args retType

typedVariable :: Parser TypedVariable
typedVariable = do
  name <- identifierToken
  varType <- typeExpression
  return $ TypedVariable name varType

typeExpression :: Parser Type
typeExpression = try booleanType
                 <|> try integerType
                 <|> try doubleType
                 <?> "type"

booleanType :: Parser Type
booleanType = do
  reservedToken "Boolean"
  return Types.Boolean

integerType :: Parser Type
integerType = do
  reservedToken "Integer"
  return Types.Integer

doubleType :: Parser Type
doubleType = do
  reservedToken "Double"
  return Types.Double

toplevelExpression :: Parser DeclarationOrExpression
toplevelExpression = do
  expr <- expressionAst
  return $ Right expr

expressionAst :: Parser ExpressionAst
expressionAst = Expr.buildExpressionParser operatorTable term

operatorTable = [[unaryOperator "+" UnaryPlus,
                  unaryOperator "-" UnaryMinus,
                  unaryOperator "~" BitwiseNot,
                  unaryOperator "!" LogicalNot]
                 ,[binaryOperator "*" Times Expr.AssocLeft,
                  binaryOperator "%" Modulo Expr.AssocLeft,
                  binaryOperator "/" DividedBy Expr.AssocLeft]
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

unaryOperator symbol operator = Expr.Prefix (reservedOpToken symbol >> return (unaryOperation operator))
binaryOperator symbol operator assoc = Expr.Infix (reservedOpToken symbol >> return (binaryOperation operator)) assoc

unaryOperation :: UnaryOperator -> ExpressionAst -> ExpressionAst
unaryOperation op exp = ExpressionAst (UnaryOperation op exp) Unknown

binaryOperation :: BinaryOperator -> ExpressionAst -> ExpressionAst -> ExpressionAst
binaryOperation op left right = ExpressionAst (BinaryOperation op left right) Unknown

term :: Parser ExpressionAst
term = do
  simpleExpressionAst
  <|> parensToken expressionAst
  <?> "expression"

simpleExpressionAst :: Parser ExpressionAst
simpleExpressionAst = do
  t <- expression
  return $ ExpressionAst t Unknown

expression :: Parser Expression
expression = try boolean
             <|> try integer
             <|> try call
             <|> try conditional
             <|> variable

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
  args <- parensToken $ many expressionAst
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

parseToplevel :: String -> String -> Either ParseError [DeclarationOrExpression]
parseToplevel source s = parse (contents toplevel) source s
