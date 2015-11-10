{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Sara.Z3.Operators where

import Sara.Utils
import Sara.Z3.Utils
import Sara.Operators
import Sara.Errors
import Z3.Monad

z3UnaryOperator :: MonadZ3 z3 => UnaryOperator -> AST -> z3 AST
z3UnaryOperator UnaryPlus e  = return e
z3UnaryOperator UnaryMinus e = do
  z <- mkZero
  mkSub [z, e]
z3UnaryOperator LogicalNot e = mkNot e
z3UnaryOperator op _         = error $ "Unsupported unary operator for Z3 verifier: " ++ show op

-- | Returns the proof obligation of a bianary operator, if there is one.
proofObligation :: BinaryOperator -> Maybe (VerifierFailureType, forall z3 . MonadZ3 z3 => AST -> AST -> z3 AST)
proofObligation DividedBy = divisionByZeroObl
proofObligation Modulo    = divisionByZeroObl
proofObligation _         = Nothing

divisionByZeroObl :: Maybe (VerifierFailureType, forall z3 . MonadZ3 z3 => AST -> AST -> z3 AST)
divisionByZeroObl = Just (DivisionByZero, const mkNeZero)

-- | Returns the Z3 equivalent for binary operators and possibly their proof obligation.
z3BinaryOperator :: MonadZ3 z3 => BinaryOperator -> AST -> AST -> z3 AST
z3BinaryOperator Times           = app2 mkMul
z3BinaryOperator DividedBy       = mkDiv
z3BinaryOperator Modulo          = mkMod
z3BinaryOperator Plus            = app2 mkAdd
z3BinaryOperator Minus           = app2 mkSub
z3BinaryOperator LessThan        = mkLt
z3BinaryOperator AtMost          = mkLe
z3BinaryOperator GreaterThan     = mkGt
z3BinaryOperator AtLeast         = mkGe
z3BinaryOperator EqualTo         = mkEq
z3BinaryOperator LogicalAnd      = app2 mkAnd
z3BinaryOperator LogicalXor      = mkXor
z3BinaryOperator LogicalOr       = app2 mkOr
z3BinaryOperator Implies         = mkImplies
z3BinaryOperator ImpliedBy       = flip mkImplies
z3BinaryOperator NotEqualTo      = \a b -> mkNot =<< mkEq a b
z3BinaryOperator EquivalentTo    = mkEq
z3BinaryOperator NotEquivalentTo = \a b -> mkNot =<< mkEq a b
z3BinaryOperator op              = error $ "Unsupported binary operator for Z3 verifier: " ++ show op
