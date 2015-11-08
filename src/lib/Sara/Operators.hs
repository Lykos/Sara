module Sara.Operators where

data UnaryOperator
  = UnaryPlus
  | UnaryMinus
  | BitwiseNot
  | LogicalNot
  deriving (Eq, Ord, Show, Enum, Bounded)

unarySymbol :: UnaryOperator -> String
unarySymbol UnaryPlus  = "+"
unarySymbol UnaryMinus = "-"
unarySymbol BitwiseNot = "~"
unarySymbol LogicalNot = "!"

unaryOperators :: [UnaryOperator]
unaryOperators = enumFrom minBound

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
  | Assign
  deriving (Eq, Ord, Show, Enum, Bounded)

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
binarySymbol Assign          = "="

binaryOperators :: [BinaryOperator]
binaryOperators = enumFrom minBound

-- | Description for which value a short circuit operator is predetermined by the left side.
data PredeterminedForValue = PredeterminedForFalse | PredeterminedForTrue
                           deriving (Eq, Ord, Show, Enum, Bounded)

-- | Description which value is used if a short circuit operator is predetermined by the left side.
data ValueWhenPredetermined = LeftSideWhenPredetermined | NotLeftSideWhenPredetermined
                            deriving (Eq, Ord, Show, Enum, Bounded)

-- | Description which value is used if a short circuit operator is not predetermined by the left side.
data ValueWhenNotPredetermined = RightSideWhenNotPredetermined | NotRightSideWhenNotPredetermined
                               deriving (Eq, Ord, Show, Enum, Bounded)

class MaybeNegation a where
  isPositive :: a -> Bool

instance MaybeNegation ValueWhenPredetermined where
  isPositive LeftSideWhenPredetermined = True
  isPositive NotLeftSideWhenPredetermined = False

instance MaybeNegation ValueWhenNotPredetermined where
  isPositive RightSideWhenNotPredetermined = True
  isPositive NotRightSideWhenNotPredetermined = False

-- | Description of the behavior of a short circuit operator.
data ShortCircuitKind
  = ShortCircuitKind { predeterminedForValue :: PredeterminedForValue
                     , valueWhenPredetermined :: ValueWhenPredetermined
                     , valueWhenNotPredetermined :: ValueWhenNotPredetermined }
  deriving (Eq, Ord, Show)

shortCircuitKind :: BinaryOperator -> Maybe ShortCircuitKind
shortCircuitKind LogicalAnd = Just $ ShortCircuitKind PredeterminedForFalse LeftSideWhenPredetermined RightSideWhenNotPredetermined
shortCircuitKind LogicalOr  = Just $ ShortCircuitKind PredeterminedForTrue LeftSideWhenPredetermined RightSideWhenNotPredetermined
shortCircuitKind Implies    = Just $ ShortCircuitKind PredeterminedForFalse NotLeftSideWhenPredetermined RightSideWhenNotPredetermined
shortCircuitKind ImpliedBy  = Just $ ShortCircuitKind PredeterminedForTrue LeftSideWhenPredetermined NotRightSideWhenNotPredetermined
shortCircuitKind _          = Nothing

class Operator o where
  symbol :: o -> String

instance Operator BinaryOperator where
  symbol = binarySymbol

instance Operator UnaryOperator where
  symbol = unarySymbol
