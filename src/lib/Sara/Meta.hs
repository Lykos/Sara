{-# LANGUAGE PatternSynonyms #-}

module Sara.Meta where

import Text.Parsec.Pos
import qualified Sara.Syntax as S
import Sara.Types

newtype NodeMeta
  = NodeMeta { nodePos :: SourcePos }
  deriving (Eq, Ord, Show)

newtype ExpressionMeta
  = ExpressionMeta { expTyp :: Type }
  deriving (Eq, Ord, Show)

type Id = Int

data FunctionMeta =
  FunctionMeta { funcSymPure :: Bool
               , funcSymName :: S.Name
               , funcSymid :: Id }
  deriving (Eq, Ord, Show)

data VariableMeta =
  VariableMeta { varSymName :: S.Name
               , varSymid :: Id }
  deriving (Eq, Ord, Show)

type ParserProgram = S.Program () () () NodeMeta

type ParserSignature = S.Signature () () () NodeMeta

type ParserExpression = S.Expression () () () NodeMeta

type ParserDeclaration = S.Declaration () () () NodeMeta

type ParserTypedVariable = S.TypedVariable () NodeMeta

type TypeCheckerProgram = S.Program () () ExpressionMeta NodeMeta

type TypeCheckerSignature = S.Signature () () ExpressionMeta NodeMeta

type TypeCheckerExpression = S.Expression () () ExpressionMeta NodeMeta

type TypeCheckerDeclaration = S.Declaration () () ExpressionMeta NodeMeta

type TypeCheckerTypedVariable = ParserTypedVariable

type SymbolizerProgram = S.Program FunctionMeta VariableMeta ExpressionMeta NodeMeta

type SymbolizerSignature = S.Signature FunctionMeta VariableMeta ExpressionMeta NodeMeta

type SymbolizerExpression = S.Expression FunctionMeta VariableMeta ExpressionMeta NodeMeta

type SymbolizerDeclaration = S.Declaration FunctionMeta VariableMeta ExpressionMeta NodeMeta

type SymbolizerTypedVariable = S.TypedVariable VariableMeta NodeMeta

expressionTyp :: S.Expression a b ExpressionMeta d -> Type
expressionTyp = expTyp . S.expressionMeta

expressionPos :: S.Expression a b c NodeMeta -> SourcePos
expressionPos = nodePos . S.nodeMeta

typedVarPos :: S.TypedVariable b NodeMeta -> SourcePos
typedVarPos = nodePos . S.nodeMeta

signaturePos :: S.Signature a b c NodeMeta -> SourcePos
signaturePos = nodePos . S.nodeMeta

declarationPos :: S.Declaration a b c NodeMeta -> SourcePos
declarationPos = nodePos . S.nodeMeta

pattern Typed t <- (ExpressionMeta t, _)
