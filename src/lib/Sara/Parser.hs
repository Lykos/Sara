module Sara.Parser (
  Sara.Parser.parse
  , Sara.Parser.signature
  , typeExpression) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor.Identity

import qualified Text.Parsec.Expr as Expr
import qualified Sara.Syntax as S
import qualified Sara.Types as T
import qualified Sara.Keywords as K
import Sara.Meta
import Sara.Lexer as L
import Sara.Syntax
import Sara.Types
import Sara.Operators
import Sara.Errors

declaration :: Parser ParserDeclaration
declaration = function
              <|> extern
              <?> "declaration"

function :: Parser ParserDeclaration
function = addNodeMeta $ S.Function <$> Sara.Parser.signature <*> bracedExpression

extern :: Parser ParserDeclaration
extern = addNodeMeta $ do
  keyword K.Extern
  sig <- Sara.Parser.signature
  return $ Extern sig

signature :: Parser ParserSignature
signature = do
  pos <- getPosition
  pure <- pureKeyword
  name <- identifierToken
  args <- parensToken $ commaSep typedVariable
  colon
  retType <- typeExpression
  precs <- conditions K.Requires
  posts <- conditions K.Ensures
  return $ Signature pure name args retType precs posts () $ NodeMeta pos

pureKeyword :: Parser Bool
pureKeyword = (keyword K.Function >> return True)
              <|> (keyword K.Method >> return False)

conditions :: K.Keyword -> Parser [ParserExpression]
conditions word = conditions' word <|> return []
  where conditions' word = do
          -- Note that we cannot use semiSep since that one doesn't handle a semicolon that does NOT belong to the conditions correctly.
          keyword word
          cond <- expression
          conds <- try (semi >> conditions' word) <|> return []
          return (cond:conds)

typedVariable :: Parser ParserTypedVariable
typedVariable = do
  pos <- getPosition
  name <- identifierToken
  colon
  varType <- typeExpression
  return $ TypedVariable name varType () $ NodeMeta pos

typeExpression :: Parser Type
typeExpression = unitType
                 <|> booleanType
                 <|> integerType
                 <|> doubleType
                 <?> "type"

unitType :: Parser Type
unitType = L.symbol "Unit" >> return T.Unit

booleanType :: Parser Type
booleanType = L.symbol "Boolean" >> return T.Boolean

integerType :: Parser Type
integerType = L.symbol "Integer" >> return T.Integer

doubleType :: Parser Type
doubleType = L.symbol "Double" >> return T.Double

expression :: Parser ParserExpression
expression = Expr.buildExpressionParser operatorTable term

operatorTable :: [[Expr.Operator String () Data.Functor.Identity.Identity ParserExpression]]
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

unaryOperator :: UnaryOperator -> Expr.Operator String () Data.Functor.Identity.Identity ParserExpression
unaryOperator operator = Expr.Prefix (operation (unarySymbol operator) (unaryOperation operator))

binaryOperator :: BinaryOperator -> Expr.Assoc -> Expr.Operator String () Data.Functor.Identity.Identity ParserExpression
binaryOperator operator = Expr.Infix (operation (binarySymbol operator) (binaryOperation operator))

operation :: String -> (NodeMeta -> a) -> Parser a
operation symbol op = addNodeMeta $ do
  reservedOpToken symbol
  return op

unaryOperation :: UnaryOperator -> NodeMeta -> ParserExpression -> ParserExpression
unaryOperation op meta exp = UnaryOperation op exp () meta

binaryOperation :: BinaryOperator -> NodeMeta -> ParserExpression -> ParserExpression -> ParserExpression
binaryOperation op meta left right = BinaryOperation op left right () meta

term :: Parser ParserExpression
term = simpleExpression
       <|> parensToken expression

addNodeMeta :: Parser (NodeMeta -> a) -> Parser a
addNodeMeta parser = do
  pos <- getPosition
  ast <- parser
  return $ ast $ NodeMeta pos

addExpressionMeta :: Parser (Incomplete a) -> Parser a
addExpressionMeta parser = addNodeMeta $ parser <*> pure ()

simpleExpression :: Parser ParserExpression
simpleExpression = addExpressionMeta $
                   try unit
                   <|> boolean
                   <|> try double
                   <|> try integer
                   <|> while
                   <|> conditional
                   <|> block
                   <|> assert
                   <|> assume
                   <|> assertAndCollapse
                   <|> varDef
                   <|> try call
                   <|> variable

type Incomplete a = () -> NodeMeta -> a
type IncompleteExpression = Incomplete ParserExpression

unit :: Parser IncompleteExpression
unit = L.symbol "()" >> return S.Unit

boolean :: Parser IncompleteExpression
boolean = (keyword K.True >> return (S.Boolean True))
          <|> (keyword K.False >> return (S.Boolean False))

integer :: Parser IncompleteExpression
integer = do
  n <- integerToken
  return $ S.Integer n

double :: Parser IncompleteExpression
double = do
  d <- doubleToken
  return $ S.Double d

conditional :: Parser IncompleteExpression
conditional = do
  keyword K.If
  cond <- expression
  keyword K.Then
  ifExpr <- expression
  keyword K.Else
  thenExpr <- expression
  return $ Conditional cond ifExpr thenExpr

block :: Parser IncompleteExpression
block = do
  pos <- getPosition
  exps <- bracesToken $ semiSep expression
  return $ if null exps then
             Block [] (S.Unit () $ NodeMeta pos)
           else
             Block (init exps) (last exps)

while :: Parser IncompleteExpression
while = do
  keyword K.While
  cond <- parensToken $ expression
  invs <- conditions K.Invariant
  body <- bracedExpression
  return $ While invs cond body

assert :: Parser IncompleteExpression
assert = assertion K.Assert Assert

assume :: Parser IncompleteExpression
assume = assertion K.Assume Assume

assertAndCollapse :: Parser IncompleteExpression
assertAndCollapse = assertion K.AssertAndCollapse AssertAndCollapse

assertion :: K.Keyword -> AssertionKind -> Parser IncompleteExpression
assertion word kind = do
  keyword word
  cond <- expression
  return $ Assertion kind cond

varDef :: Parser IncompleteExpression
varDef = do
  isVal <- (keyword K.Val >> return True) <|> (keyword K.Var >> return False)
  var <- typedVariable
  reservedOpToken "="
  rhs <- expression
  return $ VarDef var isVal rhs

call :: Parser IncompleteExpression
call = do
  name <- identifierToken
  args <- parensToken $ commaSep expression
  return $ Call name args ()

variable :: Parser IncompleteExpression
variable = do
  var <- identifierToken
  return $ Variable var ()

bracedExpression :: Parser ParserExpression
bracedExpression = simplifyBlock <$> addExpressionMeta block
  where simplifyBlock :: ParserExpression -> ParserExpression
        simplifyBlock (Block [] e _ _) = e
        simplifyBlock b                = b

contents :: Parser a -> Parser a
contents p = do
  whiteSpaceToken
  r <- p
  eof
  return r

toplevel :: Parser ParserProgram
toplevel = addNodeMeta $ do
  program <- semiSep declaration
  return $ Program program

parse :: String -> String -> ErrorOr ParserProgram
parse source s = case Text.Parsec.parse (contents toplevel) source s of
  (Left err) -> parseError err
  (Right p)  -> return p
