{-# LANGUAGE PatternSynonyms #-}

module Sara.Meta where

import Text.Parsec.Pos
import qualified Sara.Syntax as S
import Sara.Types

newtype NodeMeta
  = NodeMeta { nodePos :: SourcePos }
  deriving (Eq, Ord, Show)

newtype TypMeta
  = TypMeta { typTyp :: Type }
  deriving (Eq, Ord, Show)

data ExpressionMeta
  = ExpressionMeta { expTyp :: Type, expPure :: Bool }
  deriving (Eq, Ord, Show)

type Id = Int

data FunctionMeta =
  FunctionMeta { funcSymPure :: Bool
               , funcSymArgTypes :: [Type]
               , funcSymRetType :: Type 
               , funcSymName :: S.Name
               , funcSymid :: Id}
  deriving (Eq, Ord, Show)

data VariableMeta =
  VariableMeta { varSymType :: Type
               , varSymName :: S.Name
               , varSymId :: Id}
  deriving (Eq, Ord, Show)

type ParserProgram = S.Program () () () NodeMeta

type ParserSignature = S.Signature () () () NodeMeta

type ParserExpression = S.Expression () () () NodeMeta

type ParserDeclaration = S.Declaration () () () NodeMeta

type ParserTypedVariable = S.TypedVariable () NodeMeta

type TypeCheckerProgram = S.Program () () TypMeta NodeMeta

type TypeCheckerSignature = S.Signature () () TypMeta NodeMeta

type TypeCheckerExpression = S.Expression () () TypMeta NodeMeta

type TypeCheckerDeclaration = S.Declaration () () TypMeta NodeMeta

type TypeCheckerTypedVariable = ParserTypedVariable

type SymbolizerProgram = S.Program FunctionMeta VariableMeta TypMeta NodeMeta

type SymbolizerSignature = S.Signature FunctionMeta VariableMeta TypMeta NodeMeta

type SymbolizerExpression = S.Expression FunctionMeta VariableMeta TypMeta NodeMeta

type SymbolizerDeclaration = S.Declaration FunctionMeta VariableMeta TypMeta NodeMeta

type SymbolizerTypedVariable = S.TypedVariable VariableMeta NodeMeta

type PureCheckerProgram = S.Program FunctionMeta VariableMeta ExpressionMeta NodeMeta

type PureCheckerSignature = S.Signature FunctionMeta VariableMeta ExpressionMeta NodeMeta

type PureCheckerExpression = S.Expression FunctionMeta VariableMeta ExpressionMeta NodeMeta

type PureCheckerDeclaration = S.Declaration FunctionMeta VariableMeta ExpressionMeta NodeMeta

type PureCheckerTypedVariable = S.TypedVariable VariableMeta NodeMeta

expressionTyp' :: S.Expression a b TypMeta d -> Type
expressionTyp' = typTyp . S.expressionMeta

expressionTyp :: S.Expression a b ExpressionMeta d -> Type
expressionTyp = expTyp . S.expressionMeta

expressionPure :: S.Expression a b ExpressionMeta d -> Bool
expressionPure = expPure . S.expressionMeta

expressionPos :: S.Expression a b c NodeMeta -> SourcePos
expressionPos = nodePos . S.nodeMeta

typedVarPos :: S.TypedVariable b NodeMeta -> SourcePos
typedVarPos = nodePos . S.nodeMeta

signaturePos :: S.Signature a b c NodeMeta -> SourcePos
signaturePos = nodePos . S.nodeMeta

declarationPos :: S.Declaration a b c NodeMeta -> SourcePos
declarationPos = nodePos . S.nodeMeta

pattern Typed t <- (ExpressionMeta t _, _)
