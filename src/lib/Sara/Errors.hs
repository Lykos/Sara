module Sara.Errors( ErrorOr
                  , Error(..)
                  , PositionedError(..)
                  , UnknownElement(..)
                  , MismatchType(..)
                  , RedeclaredElement(..)
                  , SymbolicExecutionStart(..)
                  , PureContext(..)
                  , VerifierFailureType(..)
                  , VerifierFailureModel(..)
                  , FunctionOrMethod(..)
                  , showError
                  , parseError
                  , unknownVariable
                  , unknownFunction
                  , unknownUnOp
                  , unknownBinOp
                  , noMain
                  , invalidMainRetType
                  , invalidRetType
                  , impureExpression
                  , invalidMainArgs
                  , redeclaredFunction
                  , redeclaredArgument
                  , mismatchingCondTypes
                  , invalidCondType
                  , notAssignable
                  , otherError
                  , unsolvableError
                  , resultArg
                  , functionOrMethod ) where

import Sara.Types
import qualified Sara.Types as Ty
import Sara.Operators
import Sara.Syntax ( AssertionKind(..) )
import qualified Sara.PrettyPrinter as P

import qualified Data.Text as T
import Text.Parsec.Pos
import Control.Monad.Except
import Control.Monad.Identity
import qualified Text.Parsec.Error as E

type ErrorOr a = ExceptT Error Identity a
type Name = String

data Error
  = ParseError E.ParseError
  | PositionedError PositionedError SourcePos
  | NoMain
  | OtherError String
  deriving (Eq, Show)

data PositionedError
  = UnknownElementError UnknownElement
  | TypeMismatchError MismatchType Type Type  -- Used if we have one expected, one actual type.
  | DifferentTypesError [Type]                -- Used if we have several actual types that should be equal but are different.
  | MainArgsError [Type]
  | ImpureExpressionError PureContext
  | RedeclaredElementError RedeclaredElement SourcePos
  | AssignmentError
  | VerifierError SymbolicExecutionStart SourcePos VerifierFailureModel VerifierFailureType
  | UnsolvableError SymbolicExecutionStart
  | ResultArgError
  deriving (Eq, Ord, Show)

data PureContext
  = PureFunction Name
  | PurePrecondition FunctionOrMethod
  | PurePostcondition FunctionOrMethod
  | PureAssertion AssertionKind
  | PureInvariant
  deriving (Eq, Ord, Show)

data FunctionOrMethod
  = Function Name
  | Method Name
  deriving (Eq, Ord, Show)

data UnknownElement
  = UnknownUnOp UnaryOperator Type
  | UnknownBinOp BinaryOperator Type Type
  | UnknownVariable Name
  | UnknownFunction Name [Type]
  deriving (Eq, Ord, Show)

data MismatchType
  = Condition
  | ReturnType
  | MainReturnType
  deriving (Eq, Ord, Show)

data RedeclaredElement
  = RedeclaredFunction FunctionOrMethod [Type]
  | RedeclaredArgument Name FunctionOrMethod
  deriving (Eq, Ord, Show)

data SymbolicExecutionStart
  = AfterMethodEntry
  | AfterLoopEntry
  | AfterLoopExit
  | AfterAssertAndCollapse
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Assignment of variables that cause the failure.
data VerifierFailureModel
  = VerifierFailureModel [(Name, String)]  -- TODO Use something better than string.
  deriving (Eq, Ord, Show)

data VerifierFailureType
  = DivisionByZero
  | PreconditionViolation FunctionOrMethod
  | PostconditionViolation
  | AssertionViolation
  | LoopEntryInvariantViolation
  | LoopExitInvariantViolation
  deriving (Eq, Ord, Show)

showError :: String -> Error -> String
showError input err = T.unpack $ renderError (T.pack input) err

renderError :: T.Text -> Error -> T.Text
renderError _ (ParseError err)              = T.pack $ show err
renderError _ NoMain                        = T.pack "No main method found."
renderError _ (OtherError err)              = T.pack err
renderError input (PositionedError err pos) = T.unlines $ renderMessageLine err pos : renderExampleLines input pos
  where renderMessageLine :: PositionedError -> SourcePos -> T.Text
        renderMessageLine err pos = T.concat [renderPosition pos, colon, space, renderPositionedError err, dot]
        renderExampleLines :: T.Text -> SourcePos -> [T.Text]
        renderExampleLines input pos = [extractLine (sourceLine pos) input, indicatorLine]
        posSpaces pos = spaces $ sourceColumn pos - 1
        indicatorLine = T.append (posSpaces pos) indicator

renderPositionedError :: PositionedError -> T.Text
renderPositionedError (UnknownElementError elem)                                   =
  T.append (T.pack "Unknown ") (renderUnknownElement elem)
renderPositionedError (TypeMismatchError _ expectedType actualType)                =
  T.concat [T.pack "Expected ", pretty expectedType, T.pack " but got ", pretty actualType]
renderPositionedError (DifferentTypesError types)                                  =
  T.append (T.pack "Expected equal types but got ") (renderTypes types)
renderPositionedError (MainArgsError args)                                         =
  T.append (T.pack "Expected no arguments for main function but got ") (renderTypes args)
renderPositionedError (ImpureExpressionError context)                              =
  T.append (T.pack "Got impure expression in ") (renderPureContext context)
renderPositionedError (RedeclaredElementError redeclaredElement originalPos)       =
  T.concat [T.pack "Redeclared ", renderRedeclaredElement redeclaredElement, T.pack " which was already declared at ", renderPosition originalPos]
renderPositionedError AssignmentError                                              =
  T.pack "Invalid assignment target"
renderPositionedError (VerifierError symbolicExecutionStart pos model failureType) =
  T.unlines $ verifierInfoLine : modelLines ++ [failureTypeLine]
  where verifierInfoLine = T.concat [T.pack "Found a possibility to fail after ",
                                     renderSymbolicExecutionStart symbolicExecutionStart,
                                     T.pack " at ",
                                     renderPosition pos,
                                     T.pack ", when we have the following values for variables:"]
        modelLines = renderModel model
        failureTypeLine = renderFailureType failureType
renderPositionedError (UnsolvableError symbolicExecutionStart)                       =
  T.concat [T.pack "Couldn't determine whether it is possible to fail after ", renderSymbolicExecutionStart symbolicExecutionStart]
renderPositionedError ResultArgError                                                 =
  T.pack "The name \"result\" is not allowed for arguments."

renderModel :: VerifierFailureModel -> [T.Text]
renderModel (VerifierFailureModel model) = map renderModelElement model
  where renderModelElement (name, value) = spaceSep [T.pack name, equals, T.pack value]

renderFailureType :: VerifierFailureType -> T.Text
renderFailureType DivisionByZero               = T.pack "Division by zero"
renderFailureType (PreconditionViolation func) = T.concat [T.pack "precondition of ", renderFunction func, T.pack " violated"]
renderFailureType PostconditionViolation       = T.pack "postcondition violated"
renderFailureType AssertionViolation           = T.pack "assertion violated"
renderFailureType LoopEntryInvariantViolation  = T.pack "invariant violated at loop entry"
renderFailureType LoopExitInvariantViolation   = T.pack "loop invariant not preserved"

renderPureContext :: PureContext -> T.Text
renderPureContext (PureFunction name)               = T.append (T.pack "pure function") (T.pack name)
renderPureContext (PurePrecondition func)           = T.append (T.pack "precondition of ") (renderFunction func)
renderPureContext (PurePostcondition func)          = T.append (T.pack "postcondition of ") (renderFunction func)
renderPureContext (PureAssertion Assert)            = T.pack "assert"
renderPureContext (PureAssertion Assume)            = T.pack "assume"
renderPureContext (PureAssertion AssertAndCollapse) = T.pack "assertAndCollapse"
renderPureContext PureInvariant                     = T.pack "invariant"

renderFunction :: FunctionOrMethod -> T.Text
renderFunction (Function name) = T.append (T.pack "function ") (T.pack name)
renderFunction (Method name)   = T.append (T.pack "method ") (T.pack name)

renderSymbolicExecutionStart :: SymbolicExecutionStart -> T.Text
renderSymbolicExecutionStart AfterMethodEntry       = T.pack "method entry"
renderSymbolicExecutionStart AfterLoopEntry         = T.pack "loop entry"
renderSymbolicExecutionStart AfterLoopExit          = T.pack "loop exit"
renderSymbolicExecutionStart AfterAssertAndCollapse = T.pack "assertAndCollapse"

renderTypes :: [Type] -> T.Text
renderTypes = commaSep . map pretty

renderRedeclaredElement :: RedeclaredElement -> T.Text
renderRedeclaredElement (RedeclaredFunction func argTypes) = T.append (renderFunction func) (parens $ renderTypes argTypes)
renderRedeclaredElement (RedeclaredArgument name func)     = T.concat [T.pack "argument ", T.pack name, T.pack " in ", renderFunction func]

renderUnknownElement :: UnknownElement -> T.Text
renderUnknownElement (UnknownUnOp name typ)                 = renderUnknownTyped "unary operator" (unarySymbol name) [typ]
renderUnknownElement (UnknownBinOp name leftType rightType) = renderUnknownTyped "binary operator" (binarySymbol name) [leftType, rightType]
renderUnknownElement (UnknownVariable name)                 = renderUnknown "variable" name
renderUnknownElement (UnknownFunction name types)           = renderUnknownTyped "function or method" name types

renderUnknown :: String -> String -> T.Text
renderUnknown elementType name = spaceSep $ map T.pack [elementType, name]

renderUnknownTyped :: String -> String -> [Type] -> T.Text
renderUnknownTyped elementType name = renderUnknownTyped' $ renderUnknown elementType name
  where renderUnknownTyped' name []    = name
        renderUnknownTyped' name types = T.concat [name, T.pack " for types ", renderTypes types]

extractLine :: Int -> T.Text -> T.Text
extractLine n input = T.lines input !! (n - 1)

renderPosition :: SourcePos -> T.Text
renderPosition pos = colonSep $ posComponents pos
  where posComponents :: SourcePos -> [T.Text]
        posComponents pos = map (T.pack . ($ pos)) [sourceName, show . sourceLine, show . sourceColumn]

indicator :: T.Text
indicator = T.singleton '^'

colon :: T.Text
colon = T.singleton ':'

equals :: T.Text
equals = T.singleton '='

spaces :: Int -> T.Text
spaces = flip T.replicate space

space :: T.Text
space = T.singleton ' '

commaSpace :: T.Text
commaSpace = T.pack ", "

commaSep :: [T.Text] -> T.Text
commaSep = T.intercalate commaSpace

colonSep :: [T.Text] -> T.Text
colonSep = T.intercalate colon

spaceSep :: [T.Text] -> T.Text
spaceSep = T.intercalate space

parens :: T.Text -> T.Text
parens text = T.concat [T.singleton '(', text, T.singleton ')']

dot :: T.Text
dot = T.singleton '.'

pretty :: P.Pretty a => a -> T.Text
pretty = T.pack . P.prettyRender

parseError :: Monad m => E.ParseError -> ExceptT Error m a
parseError err = throwError $ ParseError err

positionedError :: Monad m => PositionedError -> SourcePos -> ExceptT Error m a
positionedError f pos = throwError $ PositionedError f pos

unknownElementError :: Monad m => UnknownElement -> SourcePos -> ExceptT Error m a
unknownElementError = positionedError . UnknownElementError

unknownUnOp :: Monad m => UnaryOperator -> Type -> SourcePos -> ExceptT Error m a
unknownUnOp op t = unknownElementError $ UnknownUnOp op t

unknownBinOp :: Monad m => BinaryOperator -> Type -> Type -> SourcePos -> ExceptT Error m a
unknownBinOp op s t = unknownElementError $ UnknownBinOp op s t

unknownVariable :: Monad m => Name -> SourcePos -> ExceptT Error m a
unknownVariable name = unknownElementError $ UnknownVariable name

unknownFunction :: Monad m => Name -> [Type] -> SourcePos -> ExceptT Error m a
unknownFunction name argTypes = unknownElementError $ UnknownFunction name argTypes

invalidCondType :: Monad m => Type -> SourcePos -> ExceptT Error m a
invalidCondType t = positionedError $ TypeMismatchError Condition Ty.Boolean t

invalidRetType :: Monad m => Type -> Type -> SourcePos -> ExceptT Error m a
invalidRetType s t = positionedError $ TypeMismatchError ReturnType s t

mismatchingCondTypes :: Monad m => Type -> Type -> SourcePos -> ExceptT Error m a
mismatchingCondTypes s t = positionedError $ DifferentTypesError [s, t]

redeclaredFunction :: Monad m => FunctionOrMethod -> [Type] -> SourcePos -> SourcePos -> ExceptT Error m a
redeclaredFunction func argTypes pos =  positionedError $ RedeclaredElementError (RedeclaredFunction func argTypes) pos

redeclaredArgument :: Monad m => Name -> FunctionOrMethod -> SourcePos -> SourcePos -> ExceptT Error m a
redeclaredArgument name func pos =  positionedError $ RedeclaredElementError (RedeclaredArgument name func) pos

invalidMainArgs :: Monad m => [Type] -> SourcePos -> ExceptT Error m a
invalidMainArgs = positionedError . MainArgsError

invalidMainRetType :: Monad m => Type -> SourcePos -> ExceptT Error m a
invalidMainRetType t = positionedError $ TypeMismatchError MainReturnType Ty.Integer t

noMain :: Monad m => ExceptT Error m a
noMain = throwError NoMain

impureExpression :: Monad m => PureContext -> SourcePos -> ExceptT Error m a
impureExpression name = positionedError $ ImpureExpressionError name

notAssignable :: Monad m => SourcePos -> ExceptT Error m a
notAssignable = positionedError AssignmentError

otherError :: Monad m => String -> ExceptT Error m a
otherError s = throwError $ OtherError s

unsolvableError :: Monad m => SymbolicExecutionStart -> SourcePos -> ExceptT Error m a
unsolvableError start = positionedError $ UnsolvableError start

resultArg :: Monad m => SourcePos -> ExceptT Error m a
resultArg = positionedError ResultArgError

functionOrMethod :: Bool -> Name -> FunctionOrMethod
functionOrMethod True name  = Function name
functionOrMethod False name = Method name
