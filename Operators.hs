module Operators where

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
