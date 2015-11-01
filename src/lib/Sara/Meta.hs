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

expressionTyp :: S.Expression a b TypeCheckerExpressionMeta d -> Type
expressionTyp = expTyp . S.expressionMeta

expressionPos :: S.Expression a b c ParserNodeMeta -> SourcePos
expressionPos = nodePos . S.nodeMeta

signaturePos :: S.Signature a b c ParserNodeMeta -> SourcePos
signaturePos = nodePos . S.nodeMeta

declarationPos :: S.Declaration a b c ParserNodeMeta -> SourcePos
declarationPos = nodePos . S.nodeMeta
