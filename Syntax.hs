module Syntax where

type Name = String
data UnaryOperator
  = UnaryPlus
  | UnaryMinus
  | BitwiseNot
  | LogicalNot
  deriving (Eq, Ord, Show)

data BinaryOperator
  = Times
  | Divide
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
  deriving (Eq, Ord, Show)

data Declaration
  = Function Name [Expression] Expression
  | Extern Name [Expression]
  deriving (Eq, Ord, Show)

data Expression
  = Integer Integer
  | UnaryOperation UnaryOperator Expression
  | BinaryOperation BinaryOperator Expression Expression
  | Variable Name
  | Call Name [Expression]
  | Conditional Expression Expression Expression
  deriving (Eq, Ord, Show)
