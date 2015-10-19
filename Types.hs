module Types where

import Operators
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
typedUnOps = Map.fromList [ (TypedUnOp UnaryPlus Types.Integer, Types.Integer)
                          , (TypedUnOp UnaryPlus Types.Double, Types.Double)
                          , (TypedUnOp UnaryMinus Types.Integer, Types.Integer)
                          , (TypedUnOp UnaryMinus Types.Double, Types.Double)
                          , (TypedUnOp BitwiseNot Types.Integer, Types.Integer)
                          , (TypedUnOp LogicalNot Types.Boolean, Types.Boolean)]

data TypedBinOp
  = TypedBinOp BinaryOperator Type Type
  deriving (Eq, Ord, Show)

typedBinOps :: Map.Map TypedBinOp Type
typedBinOps = Map.fromList $
              map (\op -> (TypedBinOp op Types.Integer Types.Integer, Types.Integer)) intBinOps
              ++ (map (\op -> (TypedBinOp op Types.Integer Types.Integer, Types.Integer)) intDoubleBinOps)
              ++ (map (\op -> (TypedBinOp op Types.Double Types.Double, Types.Double)) intDoubleBinOps)
              ++ (map (\op -> (TypedBinOp op Types.Integer Types.Integer, Types.Boolean)) relOps)
              ++ (map (\op -> (TypedBinOp op Types.Double Types.Double, Types.Boolean)) relOps)
              ++ (map (\op -> (TypedBinOp op Types.Boolean Types.Boolean, Types.Boolean)) boolOps)
  where intBinOps = [LeftShift, RightShift, BitwiseAnd, BitwiseXor, BitwiseOr]
        intDoubleBinOps = [Times, DividedBy, Modulo, Plus, Minus, Assign]
        relOps = [LessThan, AtMost, GreaterThan, AtLeast, EqualTo, NotEqualTo]
        boolOps = [LogicalAnd, LogicalXor, LogicalOr, Implies, ImpliedBy, EquivalentTo, NotEquivalentTo, Assign]
