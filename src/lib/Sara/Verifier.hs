module Sara.Verifier (z3Expression) where

import Sara.Operators
import Sara.Syntax
import Sara.AstUtils
import Data.Maybe
import Z3.Monad

import qualified Data.Map.Strict as M

newtype Z3Context = Z3Context { vars :: M.Map Name AST }

z3Expression :: Z3Context -> Expression -> Z3 AST
z3Expression _ (Boolean b _ _)              = mkBool b
z3Expression _ (Integer n _ _)              = mkInteger n
z3Expression c (UnaryOperation op e _ _)    = do
  e' <- z3Expression c e
  z3UnOp op e'
z3Expression c (BinaryOperation op l r _ _) = do
  l' <- z3Expression c l
  r' <- z3Expression c r
  z3BinOp op l' r'
z3Expression c (Variable v _ _)             = return $ fromJust $ v `M.lookup` vars c
z3Expression c (Conditional b t e _ _)      = do
  b' <- z3Expression c b
  t' <- z3Expression c t
  e' <- z3Expression c e
  mkIte b' t' e'

z3Zero :: Z3 AST
z3Zero = mkInteger 0

z3UnOp :: UnaryOperator -> AST -> Z3 AST
z3UnOp UnaryPlus e  = return e
z3UnOp UnaryMinus e = do
  z <- z3Zero
  mkSub [z, e]
z3UnOp LogicalNot e = mkNot e

liftPair :: ([a] -> b) -> a -> a -> b
liftPair f a b = f [a, b]

z3BinOp :: BinaryOperator -> AST -> AST -> Z3 AST
z3BinOp Times           = liftPair mkMul
z3BinOp DividedBy       = \a b -> do
  z <- z3Zero
  assert =<< mkNot =<< mkEq b z
  mkDiv a b
z3BinOp Modulo          = \a b -> do
  z <- z3Zero
  assert =<< mkNot =<< mkEq b z
  mkDiv a b
z3BinOp Plus            = liftPair mkAdd
z3BinOp Minus           = liftPair mkSub
z3BinOp LessThan        = mkLt
z3BinOp AtMost          = mkLe
z3BinOp GreaterThan     = mkGt
z3BinOp AtLeast         = mkGe
z3BinOp EqualTo         = mkEq
z3BinOp NotEqualTo      = \a b ->
  mkNot =<< mkEq a b
z3BinOp LogicalAnd      = liftPair mkAnd
z3BinOp LogicalXor      = mkXor
z3BinOp LogicalOr       = liftPair mkOr
z3BinOp Implies         = mkImplies
z3BinOp ImpliedBy       = flip mkImplies
z3BinOp EquivalentTo    = mkEq
z3BinOp NotEquivalentTo = \a b ->
  mkNot =<< mkEq a b
