module Sara.Errors( ErrorOr
                  , Error(..)
                  , PositionedError(..)
                  , UnknownElement(..)
                  , MismatchType(..)
                  , RedeclaredElement(..)
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
                  , mismatchingCondTypes
                  , invalidCondType
                  , notAssignable
                  , otherError
                  , verifierError ) where

import Sara.Types
import qualified Sara.Types as Ty
import Sara.Syntax
import Sara.Operators
import qualified Sara.PrettyPrinter as P

import qualified Data.Text as T
import Text.Parsec.Pos
import Control.Monad.Except
import Control.Monad.Identity
import qualified Text.Parsec.Error as E

type ErrorOr a = ExceptT Error Identity a

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
  | ImpureExpressionError Name
  | RedeclaredElementError RedeclaredElement SourcePos
  | AssignmentError
  deriving (Eq, Show)

data UnknownElement
  = UnknownUnOp UnaryOperator Type
  | UnknownBinOp BinaryOperator Type Type
  | UnknownVariable Name
  | UnknownFunction Name [Type]
  deriving (Eq, Show)

data MismatchType
  = Condition
  | ReturnType
  | MainReturnType
  deriving (Eq, Show)

data RedeclaredElement
  = RedeclaredFunction Name [Type]
  deriving (Eq, Show)

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
renderPositionedError (UnknownElementError elem)                             =
  T.append (T.pack "Unknown ") (renderUnknownElement elem)
renderPositionedError (TypeMismatchError _ expectedType actualType)          =
  T.concat [T.pack "Expected ", pretty expectedType, T.pack " but got ", pretty actualType]
renderPositionedError (DifferentTypesError types)                            =
  T.append (T.pack "Expected equal types but got ") (renderTypes types)
renderPositionedError (MainArgsError args)                                   =
  T.append (T.pack "Expected no arguments for main function but got ") (renderTypes args)
renderPositionedError (ImpureExpressionError name)                           =
  T.append (T.pack "Got impure expression in pure context ") (T.pack name)
renderPositionedError (RedeclaredElementError redeclaredElement originalPos) =
  T.concat [T.pack "Redeclared ", renderRedeclaredElement redeclaredElement, T.pack " which was already declared at ", renderPosition originalPos]
renderPositionedError AssignmentError                                        =
  T.pack "Invalid assignment target"

renderTypes :: [Type] -> T.Text
renderTypes = commaSep . map pretty

renderRedeclaredElement :: RedeclaredElement -> T.Text
renderRedeclaredElement (RedeclaredFunction name argTypes) = T.concat [T.pack "function or method ", T.pack name, parens $ renderTypes argTypes]

renderUnknownElement :: UnknownElement -> T.Text
renderUnknownElement (UnknownUnOp name typ)                 = renderUnknownTyped "unary operator" (unarySymbol name) [typ]
renderUnknownElement (UnknownBinOp name leftType rightType) = renderUnknownTyped "binary operator" (binarySymbol name) [leftType, rightType]
renderUnknownElement (UnknownVariable name)                 = renderUnknown "variable" name
renderUnknownElement (UnknownFunction name types)           = renderUnknownTyped "function or method" name types

renderUnknown :: String -> String -> T.Text
renderUnknown elementType name = spaceSep $ map T.pack [elementType, name]

renderUnknownTyped :: String -> String -> [Type] -> T.Text
renderUnknownTyped elementType name []    = renderUnknown elementType name
renderUnknownTyped elementType name types = T.concat [renderUnknown elementType name, T.pack " for types ", renderTypes types]

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

parseError :: E.ParseError -> ErrorOr a
parseError err = throwError $ ParseError err

positionedError :: PositionedError -> SourcePos -> ErrorOr a
positionedError f pos = throwError $ PositionedError f pos

unknownElementError :: UnknownElement -> SourcePos -> ErrorOr a
unknownElementError = positionedError . UnknownElementError

unknownUnOp :: UnaryOperator -> Type -> SourcePos -> ErrorOr a
unknownUnOp op t = unknownElementError $ UnknownUnOp op t

unknownBinOp :: BinaryOperator -> Type -> Type -> SourcePos -> ErrorOr a
unknownBinOp op s t = unknownElementError $ UnknownBinOp op s t

unknownVariable :: Name -> SourcePos -> ErrorOr a
unknownVariable name = unknownElementError $ UnknownVariable name

unknownFunction :: Name -> [Type] -> SourcePos -> ErrorOr a
unknownFunction name argTypes = unknownElementError $ UnknownFunction name argTypes

invalidCondType :: Type -> SourcePos -> ErrorOr a
invalidCondType t = positionedError $ TypeMismatchError Condition Ty.Boolean t

invalidRetType :: Type -> Type -> SourcePos -> ErrorOr a
invalidRetType s t = positionedError $ TypeMismatchError ReturnType s t

mismatchingCondTypes :: Type -> Type -> SourcePos -> ErrorOr a
mismatchingCondTypes s t = positionedError $ DifferentTypesError [s, t]

redeclaredFunction :: Name -> [Type] -> SourcePos -> SourcePos -> ErrorOr a
redeclaredFunction name argTypes pos =  positionedError $ RedeclaredElementError (RedeclaredFunction name argTypes) pos

invalidMainArgs :: [Type] -> SourcePos -> ErrorOr a
invalidMainArgs = positionedError . MainArgsError

invalidMainRetType :: Type -> SourcePos -> ErrorOr a
invalidMainRetType t = positionedError $ TypeMismatchError MainReturnType Ty.Integer t

noMain :: ErrorOr a
noMain = throwError NoMain

impureExpression :: Name -> SourcePos -> ErrorOr a
impureExpression name = positionedError $ ImpureExpressionError name

notAssignable :: SourcePos -> ErrorOr a
notAssignable = positionedError AssignmentError

otherError :: String -> ErrorOr a
otherError s = throwError $ OtherError s

-- TODO Make this something better than a string!
verifierError :: Monad m => String -> ExceptT Error m a
verifierError s = throwError $ OtherError s
