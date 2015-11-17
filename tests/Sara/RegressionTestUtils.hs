module Sara.RegressionTestUtils (
  Expectation(..)
  , parseExpectation) where

import Sara.Ast.Operators
import Sara.Syntax ( AssertionKind(..) )
import qualified Sara.Errors as E
import qualified Sara.Parser as P
import qualified Sara.Lexer as L

import Data.Int
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

-- The input files can have returns directives in a comment at the top to indicate what the file is supposed to return.
-- // returns 5
-- They can also have error directives to indicate errors.
-- // errors NoMain

data Expectation
  = Returns Int64
  | Errors E.Error
  deriving (Eq, Show)

reservedOpNames :: [String]
reservedOpNames = map unarySymbol unaryOperators
                  ++ map binarySymbol binaryOperators
                  
reservedNames :: [String]
reservedNames = [ "//", "returns", "errors", "PositionedError", "UnknownElementError"
                , "UnknownUnOp", "UnknownBinOp", "UnknownVariable", "UnknownFunction"
                , "TypeMismatchError", "Condition", "ReturnType", "MainReturnType"
                , "DifferentTypesError", "MainArgsError", "ImpureExpressionError"
                , "RedeclaredElementError", "RedeclaredFunction", "AssignmentError"
                , "NoMain", "PureFunction", "PurePrecondition", "PurePostcondition"
                , "Function", "Method", "RedeclaredArgument", "PureAssertion", "Assert"
                , "Assume", "AssertAndCollapse", "PureInvariant", "ResultArg" ] ++ L.reservedNames

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef { Token.reservedNames = reservedNames
                         , Token.reservedOpNames = reservedOpNames }

integerToken :: Parser Integer
integerToken = Token.integer lexer

whiteSpaceToken :: Parser ()
whiteSpaceToken = Token.whiteSpace lexer

reservedToken :: String -> Parser ()
reservedToken = Token.reserved lexer

reservedOpToken :: String -> Parser ()
reservedOpToken = Token.reservedOp lexer

expectation :: Parser Expectation
expectation = do
  whiteSpaceToken
  reservedToken "//"
  returnExpectation <|> errorExpectation <?> "expectation"

returnExpectation :: Parser Expectation
returnExpectation = do
  reservedToken "returns"
  n <- integerToken
  return $ Returns $ fromInteger n

errorExpectation :: Parser Expectation
errorExpectation = do
  reservedToken "errors"
  err <- noMain <|> positionedError <?> "error expectation"
  return $ Errors err

positionedError :: Parser E.Error
positionedError = do
  reservedToken "PositionedError"
  err <- unpositionedError
  pos <- sourcePos
  return $ E.PositionedError err pos

unpositionedError :: Parser E.PositionedError
unpositionedError = unknownElementError
                  <|> typeMismatchError
                  <|> differentTypesError
                  <|> mainArgsError
                  <|> impureExpressionError
                  <|> redeclaredElementError
                  <|> assignmentError
                  <|> resultArgError

unknownElementError :: Parser E.PositionedError
unknownElementError = do
  reservedToken "UnknownElementError"
  el <- unknownElement
  return $ E.UnknownElementError el

unknownElement :: Parser E.UnknownElement
unknownElement = unknownUnOp
                 <|> unknownBinOp
                 <|> unknownVariable
                 <|> unknownFunction

unknownUnOp :: Parser E.UnknownElement
unknownUnOp = do
  reservedToken "UnknownUnOp"
  op <- choice (map opParser unaryOperators) <?> "unary operator"
  t <- P.typeExpression
  return $ E.UnknownUnOp op t

unknownBinOp :: Parser E.UnknownElement
unknownBinOp = do
  reservedToken "UnknownBinOp"
  op <- choice (map opParser binaryOperators) <?> "binary operator"
  s <- P.typeExpression
  t <- P.typeExpression
  return $ E.UnknownBinOp op s t

opParser :: Operator o => o -> Parser o
opParser op = reservedOpToken (symbol op) >> return op

unknownVariable :: Parser E.UnknownElement
unknownVariable = do
  reservedToken "UnknownVariable"
  name <- L.identifierToken
  return $ E.UnknownVariable name

unknownFunction :: Parser E.UnknownElement
unknownFunction = do
  reservedToken "UnknownFunction"
  name <- L.identifierToken
  t <- many P.typeExpression
  return $ E.UnknownFunction name t

typeMismatchError :: Parser E.PositionedError
typeMismatchError = do
  reservedToken "TypeMismatchError"
  m <- mismatchType
  s <- P.typeExpression
  t <- P.typeExpression
  return $ E.TypeMismatchError m s t

mismatchType :: Parser E.MismatchType
mismatchType = condition
               <|> returnType
               <|> mainReturnType
               <?> "mismatch type"

condition :: Parser E.MismatchType
condition = reservedToken "Condition" >> return E.Condition

returnType :: Parser E.MismatchType
returnType = reservedToken "ReturnType" >> return E.ReturnType

mainReturnType :: Parser E.MismatchType
mainReturnType = reservedToken "MainReturnType" >> return E.MainReturnType

differentTypesError :: Parser E.PositionedError
differentTypesError = do
  reservedToken "DifferentTypesError"
  t <- many P.typeExpression
  return $ E.DifferentTypesError t
  
mainArgsError :: Parser E.PositionedError
mainArgsError = do
  reservedToken "MainArgsError"
  t <- many P.typeExpression
  return $ E.MainArgsError t

impureExpressionError :: Parser E.PositionedError
impureExpressionError = do
  reservedToken "ImpureExpressionError"
  context <- pureContext
  return $ E.ImpureExpressionError context

pureContext :: Parser E.PureContext
pureContext = pureFunction
              <|> purePrecondition
              <|> purePostcondition
              <|> pureInvariant
              <|> pureAssertion
              <?> "pure context type"

pureFunction :: Parser E.PureContext
pureFunction = do
  reservedToken "PureFunction"
  name <- L.identifierToken
  return $ E.PureFunction name

purePrecondition :: Parser E.PureContext
purePrecondition = do
  reservedToken "PurePrecondition"
  f <- functionOrMethod
  return $ E.PurePrecondition f

purePostcondition :: Parser E.PureContext
purePostcondition = do
  reservedToken "PurePostcondition"
  f <- functionOrMethod
  return $ E.PurePostcondition f

pureInvariant :: Parser E.PureContext
pureInvariant = do
  reservedToken "PureInvariant"
  return $ E.PureInvariant

pureAssertion :: Parser E.PureContext
pureAssertion = do
  reservedToken "PureAssertion"
  k <- assertionKind
  return $ E.PureAssertion k

assertionKind :: Parser AssertionKind
assertionKind = do
  (reservedToken "Assert" >> return Assert)
  <|> (reservedToken "Assume" >> return Assume)
  <|> (reservedToken "AssertAndCollapse" >> return AssertAndCollapse)

functionOrMethod :: Parser E.FunctionOrMethod
functionOrMethod = function <|> method <?> "function or method"

function :: Parser E.FunctionOrMethod
function = do
  reservedToken "Function"
  name <- L.identifierToken
  return $ E.Function name

method :: Parser E.FunctionOrMethod
method = do
  reservedToken "Method"
  name <- L.identifierToken
  return $ E.Method name

redeclaredElementError :: Parser E.PositionedError
redeclaredElementError = do
  reservedToken "RedeclaredElementError"
  r <- redeclaredElement
  pos <- sourcePos
  return $ E.RedeclaredElementError r pos

redeclaredElement :: Parser E.RedeclaredElement
redeclaredElement = redeclaredFunction
                    <|> redeclaredArgument
                    <?> "redeclared element expectation"

redeclaredFunction :: Parser E.RedeclaredElement
redeclaredFunction = do
  reservedToken "RedeclaredFunction"
  f <- functionOrMethod
  t <- many P.typeExpression
  return $ E.RedeclaredFunction f t

redeclaredArgument :: Parser E.RedeclaredElement
redeclaredArgument = do
  reservedToken "RedeclaredArgument"
  name <- L.identifierToken
  f <- functionOrMethod
  return $ E.RedeclaredArgument name f

assignmentError :: Parser E.PositionedError
assignmentError = do
  reservedToken "AssignmentError"
  return E.AssignmentError

resultArgError :: Parser E.PositionedError
resultArgError = do
  reservedToken "ResultArg"
  return E.ResultArgError

noMain :: Parser E.Error
noMain = reservedToken "NoMain" >> return E.NoMain

sourcePos :: Parser SourcePos
sourcePos = do
  pos <- getPosition
  let name = sourceName pos
  line <- L.integerToken
  reservedOpToken ":"
  column <- L.integerToken
  return $ newPos name (fromInteger line) (fromInteger column)

parseExpectation :: String -> String -> Expectation
parseExpectation fname input = case Text.Parsec.parse expectation fname input of
  (Left err) -> error $ show err
  (Right p)  -> p
