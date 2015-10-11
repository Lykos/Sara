module Syntax where

import Data.Bifunctor
import Types
import Text.Parsec.Pos

type Name = String
data UnaryOperator
  = UnaryPlus
  | UnaryMinus
  | BitwiseNot
  | LogicalNot
  deriving (Eq, Ord, Show, Enum)

unarySymbol :: UnaryOperator -> String
unarySymbol UnaryPlus  = "+"
unarySymbol UnaryMinus = "-"
unarySymbol BitwiseNot = "~"
unarySymbol LogicalNot = "!"

unaryOperators :: [UnaryOperator]
unaryOperators = enumFrom $ toEnum 0

data BinaryOperator
  = Times
  | DividedBy
  | Modulo
  | Plus
  | Minus
  | LeftShift
  | RightShift
  | LessThan
  | AtMost
  | GreaterThan
  | AtLeast
  | EqualTo
  | NotEqualTo
  | BitwiseAnd
  | BitwiseXor
  | BitwiseOr
  | LogicalAnd
  | LogicalXor
  | LogicalOr
  | Implies
  | ImpliedBy
  | EquivalentTo
  | NotEquivalentTo
  deriving (Eq, Ord, Show, Enum)

binarySymbol :: BinaryOperator -> String
binarySymbol Times           = "*"
binarySymbol DividedBy       = "/"
binarySymbol Modulo          = "%"
binarySymbol Plus            = "+"
binarySymbol Minus           = "-"
binarySymbol LeftShift       = "<<"
binarySymbol RightShift      = ">>"
binarySymbol LessThan        = "<"
binarySymbol AtMost          = "<="
binarySymbol GreaterThan     = ">"
binarySymbol AtLeast         = ">="
binarySymbol EqualTo         = "=="
binarySymbol NotEqualTo      = "!="
binarySymbol BitwiseAnd      = "&"
binarySymbol BitwiseXor      = "^"
binarySymbol BitwiseOr       = "|"
binarySymbol LogicalAnd      = "&&"
binarySymbol LogicalXor      = "^^"
binarySymbol LogicalOr       = "||"
binarySymbol Implies         = "==>"
binarySymbol ImpliedBy       = "<=="
binarySymbol EquivalentTo    = "<==>"
binarySymbol NotEquivalentTo = "<!=>"

binaryOperators :: [BinaryOperator]
binaryOperators = enumFrom $ toEnum 0

-- Declaration AST node that contains the declaration and some metadata.
data DeclarationAst
  = DeclarationAst { decl :: Declaration
                   , declPos :: SourcePos }
  deriving (Eq, Ord, Show)

data Declaration
  = Function Signature ExpressionAst
  | Extern Signature
  deriving (Eq, Ord, Show)

signature :: Declaration -> Signature
signature (Function sig _) = sig
signature (Extern sig)     = sig

data Signature
  = Signature { funcName :: Name
              , args :: [TypedVariable]
              , returnType :: Type }
  deriving (Eq, Ord, Show)

data TypedVariable
  = TypedVariable { varName :: Name
                  , varType :: Type }
  deriving (Eq, Ord, Show)

-- Expression AST node that contains the expression and some metadata.
data ExpressionAst
  = ExpressionAst { astExp :: Expression
                  , expType :: Type
                  , expPos :: SourcePos }
  deriving (Eq, Ord, Show)

mapExpressionAstExpressionAst :: (ExpressionAst -> ExpressionAst) -> ExpressionAst -> ExpressionAst
mapExpressionAstExpressionAst f (ExpressionAst exp typ pos) = f (ExpressionAst mappedExp typ pos)
  where mappedExp = case exp of
          BinaryOperation op left right  -> BinaryOperation op (mapExpressionAstExpressionAst f left) (mapExpressionAstExpressionAst f right)
          UnaryOperation op exp          -> UnaryOperation op (mapExpressionAstExpressionAst f exp)
          Conditional cond ifExp elseExp -> Conditional (mapExpressionAstExpressionAst f cond)
                                            (mapExpressionAstExpressionAst f ifExp) (mapExpressionAstExpressionAst f elseExp)
          Call name args                 -> Call name (map (mapExpressionAstExpressionAst f) args)
          e                              -> e

mapExpressionAst :: (ExpressionAst -> ExpressionAst) -> [DeclarationOrExpression] -> [DeclarationOrExpression]
mapExpressionAst f = map (second (mapExpressionAstExpressionAst f) . first mapExpressionAstFunction)
  where mapExpressionAstFunction (DeclarationAst (Function sig body) pos) = DeclarationAst (Function sig (mapExpressionAstExpressionAst f body)) pos
        mapExpressionAstFunction d                                        = d

mapDeclarationAst :: (DeclarationAst -> DeclarationAst) -> [DeclarationOrExpression] -> [DeclarationOrExpression]
mapDeclarationAst = map . first

data Expression
  = Boolean Bool
  | Integer Integer
  | Double Double
  | UnaryOperation UnaryOperator ExpressionAst
  | BinaryOperation BinaryOperator ExpressionAst ExpressionAst
  | Variable Name
  | Call Name [ExpressionAst]
  | Conditional ExpressionAst ExpressionAst ExpressionAst
  deriving (Eq, Ord, Show)

type DeclarationOrExpression = Either DeclarationAst ExpressionAst
