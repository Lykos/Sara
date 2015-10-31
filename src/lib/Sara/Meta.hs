module Sara.Meta where

import Text.Parsec.Pos
import qualified Sara.Syntax as S
import Sara.Types

newtype ParserNodeMeta
  = ParserNodeMeta { nodePos :: SourcePos }
  deriving (Eq, Ord, Show)

newtype TypeCheckerExpressionMeta
  = TypeCheckerExpressionMeta { expTyp :: Type }
  deriving (Eq, Ord, Show)
                                
type ParserProgram = S.Program () () () ParserNodeMeta

type ParserSignature = S.Signature () () () ParserNodeMeta

type ParserExpression = S.Expression () () () ParserNodeMeta

type ParserDeclaration = S.Declaration () () () ParserNodeMeta

type ParserTypedVariable = S.TypedVariable () ParserNodeMeta

type TypeCheckerProgram = S.Program () () TypeCheckerExpressionMeta ParserNodeMeta

type TypeCheckerSignature = S.Signature () () TypeCheckerExpressionMeta ParserNodeMeta

type TypeCheckerExpression = S.Expression () () TypeCheckerExpressionMeta ParserNodeMeta

type TypeCheckerDeclaration = S.Declaration () () TypeCheckerExpressionMeta ParserNodeMeta

expressionTyp :: TypeCheckerExpression -> Type
expressionTyp = expTyp . S.expressionMeta

expressionPos :: TypeCheckerExpression -> SourcePos
expressionPos = nodePos . S.nodeMeta

signaturePos :: ParserSignature -> SourcePos
signaturePos = nodePos . S.nodeMeta

declarationPos :: ParserDeclaration -> SourcePos
declarationPos = nodePos . S.nodeMeta
