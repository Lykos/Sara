module Sara.Parser (
  Sara.Parser.parse
  , Sara.Parser.signature
  , typeExpression) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Void
import Data.Functor.Identity

import qualified Text.Parsec.Expr as Expr
import qualified Sara.Syntax as S
import qualified Sara.Types as T
import Sara.Lexer
import Sara.Syntax
import Sara.Types
import Sara.Operators
import Sara.Errors

declaration :: Parser Declaration
declaration = try function
              <|> try extern
              <?> "declaration"

function :: Parser Declaration
function = addPosition $ do
  sig <- Sara.Parser.signature
  reservedOpToken "="
  body <- expression
  return $ Function sig body

extern :: Parser Declaration
extern = addPosition $ do
  reservedToken "extern"
  sig <- Sara.Parser.signature
  return $ Extern sig

signature :: Parser Signature
signature = addPosition $ do
  pure <- pureKeyword
  name <- identifierToken
  args <- parensToken $ commaSep typedVariable
  reservedToken ":"
  retType <- typeExpression
  precs <- many precondition
  posts <- many postcondition
  return $ Signature pure name args retType precs posts

precondition :: Parser Expression
precondition = condition "requires"

postcondition :: Parser Expression
postcondition = condition "ensures"

condition :: String -> Parser Expression
condition keyword = reservedToken keyword >> expression

pureKeyword :: Parser Bool
pureKeyword = (reservedToken "function" >> return True)
              <|> (reservedToken "method" >> return False)

typedVariable :: Parser TypedVariable
typedVariable = addPosition $ do
  name <- identifierToken
  reservedToken ":"
  varType <- typeExpression
  return $ TypedVariable name varType

typeExpression :: Parser Type
typeExpression = try unitType
                 <|> try booleanType
                 <|> try integerType
                 <|> try doubleType
                 <?> "type"

unitType :: Parser Type
unitType = reservedToken "Unit" >> return T.Unit

booleanType :: Parser Type
booleanType = reservedToken "Boolean" >> return T.Boolean

integerType :: Parser Type
integerType = reservedToken "Integer" >> return T.Integer

doubleType :: Parser Type
doubleType = reservedToken "Double" >> return T.Double

expression :: Parser Expression
expression = Expr.buildExpressionParser operatorTable term

operatorTable :: [[Expr.Operator String () Data.Functor.Identity.Identity Expression]]
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
                  , binaryOperator NotEquivalentTo Expr.AssocLeft]
                , [ binaryOperator Assign Expr.AssocRight]]

unaryOperator :: UnaryOperator -> Expr.Operator String () Data.Functor.Identity.Identity Expression
unaryOperator operator = Expr.Prefix (operation (unarySymbol operator) (unaryOperation operator))

binaryOperator :: BinaryOperator -> Expr.Assoc -> Expr.Operator String () Data.Functor.Identity.Identity Expression
binaryOperator operator = Expr.Infix (operation (binarySymbol operator) (binaryOperation operator))

operation :: String -> (SourcePos -> a) -> Parser a
operation symbol op = addPosition $ do
  reservedOpToken symbol
  return op

unaryOperation :: UnaryOperator -> SourcePos -> Expression -> Expression
unaryOperation op pos exp = UnaryOperation op exp Unknown pos

binaryOperation :: BinaryOperator -> SourcePos -> Expression -> Expression -> Expression
binaryOperation op pos left right = BinaryOperation op left right Unknown pos

term :: Parser Expression
term = simpleExpression
       <|> parensToken expression
       <?> "expression"

addPosition :: Parser (SourcePos -> a) -> Parser a
addPosition parser = do
  pos <- getPosition
  ast <- parser
  return $ ast pos

addTypPosition :: Parser (Type -> SourcePos -> a) -> Parser a
addTypPosition parser = addPosition $ do
  ast <- parser
  return $ ast Unknown

simpleExpression :: Parser Expression
simpleExpression = addTypPosition $
                   try unit
                   <|> try boolean
                   <|> try double
                   <|> try integer
                   <|> try call
                   <|> try conditional
                   <|> try block
                   <|> try while
                   <|> variable

empty :: Parser Void
empty = return undefined

type UntypedExpression = Type -> SourcePos -> Expression

unit :: Parser UntypedExpression
unit = parensToken empty >> return S.Unit

boolean :: Parser UntypedExpression
boolean = (reservedToken "true" >> return (S.Boolean True))
          <|> (reservedToken "false" >> return (S.Boolean False))

integer :: Parser UntypedExpression
integer = do
  n <- integerToken
  return $ S.Integer n

double :: Parser UntypedExpression
double = do
  d <- doubleToken
  return $ S.Double d

variable :: Parser UntypedExpression
variable = do
  var <- identifierToken
  return $ Variable var

call :: Parser UntypedExpression
call = do
  name <- identifierToken
  args <- parensToken $ commaSep expression
  return $ Call name args

conditional :: Parser UntypedExpression
conditional = do
  reservedToken "if"
  cond <- expression
  reservedToken "then"
  ifExpr <- expression
  reservedToken "else"
  thenExpr <- expression
  return $ Conditional cond ifExpr thenExpr

block :: Parser UntypedExpression
block = do
  pos <- getPosition
  exps <- bracesToken $ semiSep expression
  return $ if null exps then
             Block [] (S.Unit Unknown pos)
           else
             Block (init exps) (last exps)

while :: Parser UntypedExpression
while = do
  reservedToken "while"
  cond <- expression
  bodyBlock <- addTypPosition block
  let body = transformBody bodyBlock
  return $ While cond body
  where transformBody :: Expression -> Expression
        transformBody (Block [] e _ _) = e
        transformBody b                = b

contents :: Parser a -> Parser a
contents p = do
  whiteSpaceToken
  r <- p
  eof
  return r

toplevel :: Parser Program
toplevel = addPosition $ do
  program <- semiSep declaration
  return $ Program program

parse :: String -> String -> ErrorOr Program
parse source s = case Text.Parsec.parse (contents toplevel) source s of
  (Left err) -> parseError err
  (Right p)  -> return p
