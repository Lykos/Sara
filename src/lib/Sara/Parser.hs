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
  reservedToken ":"
  retType <- typeExpression
  precs <- conditions "requires"
  posts <- conditions "ensures"
  return $ Signature pure name args retType precs posts $ mkExpMeta pos

pureKeyword :: Parser Bool
pureKeyword = (reservedToken "function" >> return True)
              <|> (reservedToken "method" >> return False)

conditions :: String -> Parser [ParserExpression]
conditions keyword = conditions' keyword <|> return []
  where conditions' keyword = do
          -- Note that we cannot use semiSep since that one doesn't handle a semicolon that does NOT belong to the conditions correctly.
          reservedToken keyword
          cond <- expression
          conds <- try (semi >> conditions' keyword) <|> return []
          return (cond:conds)

typedVariable :: Parser ParserTypedVariable
typedVariable = do
  pos <- getPosition
  name <- identifierToken
  reservedToken ":"
  varType <- typeExpression
  return $ TypedVariable name varType $ mkExpMeta pos

typeExpression :: Parser Type
typeExpression = unitType
                 <|> booleanType
                 <|> integerType
                 <|> doubleType
                 <?> "type"

unitType :: Parser Type
unitType = reservedToken "Unit" >> return T.Unit

booleanType :: Parser Type
booleanType = reservedToken "Boolean" >> return T.Boolean

integerType :: Parser Type
integerType = reservedToken "Integer" >> return T.Integer

doubleType :: Parser Type
doubleType = reservedToken "Double" >> return T.Double

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

operation :: String -> (ExpMeta -> a) -> Parser a
operation symbol op = addExpressionMeta $ do
  reservedOpToken symbol
  return op

unaryOperation :: UnaryOperator -> ExpMeta -> ParserExpression -> ParserExpression
unaryOperation op meta exp = UnaryOperation op exp meta

binaryOperation :: BinaryOperator -> ExpMeta -> ParserExpression -> ParserExpression -> ParserExpression
binaryOperation op meta left right = BinaryOperation op left right meta

term :: Parser ParserExpression
term = simpleExpression
       <|> parensToken expression

addNodeMeta :: Parser (NodeMeta -> a) -> Parser a
addNodeMeta parser = do
  pos <- getPosition
  ast <- parser
  return $ ast $ NodeMeta pos

addExpressionMeta :: Parser (ExpMeta -> a) -> Parser a
addExpressionMeta parser = do
  pos <- getPosition
  ast <- parser
  return $ ast $ mkExpMeta pos

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
                   <|> try call
                   <|> variable

type ExpMeta = ((), NodeMeta)
type UntypedExpression = ExpMeta -> ParserExpression

mkExpMeta :: SourcePos -> ExpMeta
mkExpMeta pos = ((), NodeMeta pos)

unit :: Parser UntypedExpression
unit = L.symbol "()" >> return S.Unit

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
  return $ Variable var ()

call :: Parser UntypedExpression
call = do
  name <- identifierToken
  args <- parensToken $ commaSep expression
  return $ Call name args ()

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
             Block [] (S.Unit $ mkExpMeta pos)
           else
             Block (init exps) (last exps)

while :: Parser UntypedExpression
while = do
  reservedToken "while"
  cond <- parensToken $ expression
  invs <- conditions "invariant"
  body <- bracedExpression
  return $ While invs cond body

assert :: Parser UntypedExpression
assert = assertion "assert" Assert

assume :: Parser UntypedExpression
assume = assertion "assume" Assume

assertAndCollapse :: Parser UntypedExpression
assertAndCollapse = assertion "assertAndCollapse" AssertAndCollapse

assertion :: String -> AssertionKind -> Parser UntypedExpression
assertion keyword kind = do
  reservedToken keyword
  cond <- expression
  return $ Assertion kind cond

bracedExpression :: Parser ParserExpression
bracedExpression = simplifyBlock <$> addExpressionMeta block
  where simplifyBlock :: ParserExpression -> ParserExpression
        simplifyBlock (Block [] e _) = e
        simplifyBlock b              = b

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
