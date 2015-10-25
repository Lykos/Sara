module Sara.Types where

import Sara.Operators
import qualified Data.Map.Strict as Map

data Type
  = Unit
  | Boolean
  | Integer
  | Double
  | Unknown
  deriving (Eq, Ord, Show, Enum, Bounded)

types :: [Type]
types = filter known $ enumFrom minBound
  where known :: Type -> Bool
        known = (/=) Unknown

data TypedUnOp
  = TypedUnOp UnaryOperator Type
  deriving (Eq, Ord, Show)

typedUnOps :: Map.Map TypedUnOp Type
typedUnOps = Map.fromList [ (TypedUnOp UnaryPlus Sara.Types.Integer, Sara.Types.Integer)
                          , (TypedUnOp UnaryPlus Sara.Types.Double, Sara.Types.Double)
                          , (TypedUnOp UnaryMinus Sara.Types.Integer, Sara.Types.Integer)
                          , (TypedUnOp UnaryMinus Sara.Types.Double, Sara.Types.Double)
                          , (TypedUnOp BitwiseNot Sara.Types.Integer, Sara.Types.Integer)
                          , (TypedUnOp LogicalNot Sara.Types.Boolean, Sara.Types.Boolean)]

data TypedBinOp
  = TypedBinOp BinaryOperator Type Type
  deriving (Eq, Ord, Show)

typedBinOps :: Map.Map TypedBinOp Type
typedBinOps = Map.fromList $
              map (\op -> (TypedBinOp op Sara.Types.Integer Sara.Types.Integer, Sara.Types.Integer)) intBinOps
              ++ map (\op -> (TypedBinOp op Sara.Types.Integer Sara.Types.Integer, Sara.Types.Integer)) intDoubleBinOps
              ++ map (\op -> (TypedBinOp op Sara.Types.Double Sara.Types.Double, Sara.Types.Double)) intDoubleBinOps
              ++ map (\op -> (TypedBinOp op Sara.Types.Integer Sara.Types.Integer, Sara.Types.Boolean)) relOps
              ++ map (\op -> (TypedBinOp op Sara.Types.Double Sara.Types.Double, Sara.Types.Boolean)) relOps
              ++ map (\op -> (TypedBinOp op Sara.Types.Boolean Sara.Types.Boolean, Sara.Types.Boolean)) boolOps
  where intBinOps = [LeftShift, RightShift, BitwiseAnd, BitwiseXor, BitwiseOr]
        intDoubleBinOps = [Times, DividedBy, Modulo, Plus, Minus, Assign]
        relOps = [LessThan, AtMost, GreaterThan, AtLeast, EqualTo, NotEqualTo]
        boolOps = [LogicalAnd, LogicalXor, LogicalOr, Implies, ImpliedBy, EquivalentTo, NotEquivalentTo, Assign]
