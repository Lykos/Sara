module Sara.Ast.Types where

import Sara.Ast.Operators
import qualified Data.Map.Strict as Map

data Type
  = Unit
  | Boolean
  | Integer
  | Double
  deriving (Eq, Ord, Show, Enum, Bounded)

types :: [Type]
types = enumFrom minBound

data TypedUnOp
  = TypedUnOp UnaryOperator Type
  deriving (Eq, Ord, Show)

typedUnOps :: Map.Map TypedUnOp Type
typedUnOps = Map.fromList [ (TypedUnOp UnaryPlus Sara.Ast.Types.Integer, Sara.Ast.Types.Integer)
                          , (TypedUnOp UnaryPlus Sara.Ast.Types.Double, Sara.Ast.Types.Double)
                          , (TypedUnOp UnaryMinus Sara.Ast.Types.Integer, Sara.Ast.Types.Integer)
                          , (TypedUnOp UnaryMinus Sara.Ast.Types.Double, Sara.Ast.Types.Double)
                          , (TypedUnOp BitwiseNot Sara.Ast.Types.Integer, Sara.Ast.Types.Integer)
                          , (TypedUnOp LogicalNot Sara.Ast.Types.Boolean, Sara.Ast.Types.Boolean)]

data TypedBinOp
  = TypedBinOp BinaryOperator Type Type
  deriving (Eq, Ord, Show)

typedBinOps :: Map.Map TypedBinOp Type
typedBinOps = Map.fromList $
              map (\op -> (TypedBinOp op Sara.Ast.Types.Integer Sara.Ast.Types.Integer, Sara.Ast.Types.Integer)) intBinOps
              ++ map (\op -> (TypedBinOp op Sara.Ast.Types.Integer Sara.Ast.Types.Integer, Sara.Ast.Types.Integer)) intDoubleBinOps
              ++ map (\op -> (TypedBinOp op Sara.Ast.Types.Double Sara.Ast.Types.Double, Sara.Ast.Types.Double)) intDoubleBinOps
              ++ map (\op -> (TypedBinOp op Sara.Ast.Types.Integer Sara.Ast.Types.Integer, Sara.Ast.Types.Boolean)) relOps
              ++ map (\op -> (TypedBinOp op Sara.Ast.Types.Double Sara.Ast.Types.Double, Sara.Ast.Types.Boolean)) relOps
              ++ map (\op -> (TypedBinOp op Sara.Ast.Types.Boolean Sara.Ast.Types.Boolean, Sara.Ast.Types.Boolean)) boolOps
  where intBinOps = [LeftShift, RightShift, BitwiseAnd, BitwiseXor, BitwiseOr]
        intDoubleBinOps = [Times, DividedBy, Modulo, Plus, Minus, Assign]
        relOps = [LessThan, AtMost, GreaterThan, AtLeast, EqualTo, NotEqualTo]
        boolOps = [LogicalAnd, LogicalXor, LogicalOr, Implies, ImpliedBy, EquivalentTo, NotEquivalentTo, Assign]
