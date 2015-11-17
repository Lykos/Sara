{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Sara.Z3.Operators where

import Sara.Utils
import qualified Sara.Ast.Operators as O
import qualified Sara.Errors as E
import qualified Sara.Z3.Ast as A

translateUnOp :: O.UnaryOperator -> A.Ast -> A.Ast
translateUnOp O.UnaryPlus  = id
translateUnOp O.UnaryMinus = A.UnOp A.UnMinus
translateUnOp O.LogicalNot = A.UnOp A.Not
translateUnOp op           = error $ "Unsupported unary operator for Z3 verifier: " ++ show op

-- | Returns the proof obligation of a bianary operator, if there is one.
proofObligation :: O.BinaryOperator -> Maybe (E.VerifierFailureType, A.Ast -> A.Ast -> A.Ast)
proofObligation O.DividedBy = divisionByZeroObl
proofObligation O.Modulo    = divisionByZeroObl
proofObligation _           = Nothing

divisionByZeroObl :: Maybe (E.VerifierFailureType, A.Ast -> A.Ast -> A.Ast)
divisionByZeroObl = Just (E.DivisionByZero, const neZero)

neZero :: A.Ast -> A.Ast
neZero = A.UnOp A.Not . A.BinOp A.EqualTo (A.IntConst 0)

-- | Returns the Z3 equivalent for binary operators and possibly their proof obligation.
translateBinOp :: O.BinaryOperator -> A.Ast -> A.Ast -> A.Ast
translateBinOp O.Times           = app2 (A.NaryOp A.Times)
translateBinOp O.DividedBy       = A.BinOp A.DividedBy
translateBinOp O.Modulo          = A.BinOp A.Modulo
translateBinOp O.Plus            = app2 (A.NaryOp A.Plus)
translateBinOp O.Minus           = A.BinOp A.Minus
translateBinOp O.LessThan        = A.BinOp A.LessThan
translateBinOp O.AtMost          = A.BinOp A.AtMost
translateBinOp O.GreaterThan     = A.BinOp A.GreaterThan
translateBinOp O.AtLeast         = A.BinOp A.AtLeast
translateBinOp O.EqualTo         = A.BinOp A.EqualTo
translateBinOp O.LogicalAnd      = app2 (A.NaryOp A.And)
translateBinOp O.LogicalXor      = A.BinOp A.Xor
translateBinOp O.LogicalOr       = app2 (A.NaryOp A.Or)
translateBinOp O.Implies         = (A.BinOp A.Implies)
translateBinOp O.ImpliedBy       = flip (A.BinOp A.Implies)
translateBinOp O.NotEqualTo      = \a b -> A.UnOp A.Not (A.BinOp A.EqualTo a b)
translateBinOp O.EquivalentTo    = A.BinOp A.EqualTo
translateBinOp O.NotEquivalentTo = \a b -> A.UnOp A.Not (A.BinOp A.EqualTo a b)
translateBinOp op                = error $ "Unsupported binary operator for Z3 verifier: " ++ show op
