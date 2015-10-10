module Syntax where

import Types

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
  = Function Signature Expression
  | Extern Signature
  deriving (Eq, Ord, Show)

data Signature = Signature Name [TypedVariable] Type
  deriving (Eq, Ord, Show)

data TypedVariable = TypedVariable Name Type
  deriving (Eq, Ord, Show)

data Expression
  = Boolean Bool
  | Integer Integer
  | Double Double
  | UnaryOperation UnaryOperator Expression Type
  | BinaryOperation BinaryOperator Expression Expression Type
  | Variable Name Type
  | Call Name [Expression] Type
  | Conditional Expression Expression Expression Type
  deriving (Eq, Ord, Show)
