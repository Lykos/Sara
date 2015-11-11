{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Sara.Z3.PureExpression ( z3Expression ) where

import Control.Monad.State.Strict
import Sara.PrettyPrinter
import Sara.Errors as E
import Sara.Z3.CondAst
import Sara.Z3.Utils
import Sara.Z3.Operators
import Sara.Operators
import qualified Sara.Z3.SymbolicState as S
import qualified Sara.Syntax as S
import Sara.Meta
import Z3.Monad
import Text.Parsec.Pos (SourcePos)

-- | Create an expression and its precondition and postcondition.
z3Expression :: (MonadState S.SymbolicState z3, MonadZ3 z3) => PureCheckerExpression -> z3 CondAST
z3Expression (S.Boolean b _)                = trivial =<< mkBool b
z3Expression (S.Integer n _)                = trivial =<< mkInteger n
z3Expression (S.UnaryOperation op e _)      = do
  e' <- z3Expression e
  z3UnOp op e'
z3Expression b@(S.BinaryOperation op l r _) = do
  l' <- z3Expression l
  r' <- z3Expression r
  let pos = expressionPos b
  z3BinOp pos op l' r'
z3Expression (S.Variable _ m _)             = trivial =<< S.getOrCreateVar m
z3Expression (S.Conditional c t e _)        = do
  c' <- z3Expression c
  t' <- conditionOn (ast c') =<< z3Expression t
  e' <- conditionOnNot (ast c') =<< z3Expression e
  combine3 mkIte c' t' e'
z3Expression c@(S.Call name a m _)          = do
  a' <- mapM z3Expression a
  let args = map ast a'
  (funcPre, funcPost, func) <- z3FuncDecls m
  pre <- mkApp funcPre args
  post <- mkApp funcPost args
  let pos = expressionPos c
  let failureType = PreconditionViolation (E.Function name)
  addProofObligation pre failureType pos =<< addAssumption post =<< combine (mkApp func) a'
z3Expression exp                            = error $ "Unsupported expression for pure verifier: " ++ prettyRender exp

z3UnOp :: MonadZ3 z3 => UnaryOperator -> CondAST -> z3 CondAST
z3UnOp op = liftCond $ z3UnaryOperator op

z3BinOp :: MonadZ3 z3 => SourcePos -> BinaryOperator -> CondAST -> CondAST -> z3 CondAST
z3BinOp p op a b         = do
  b' <- case shortCircuitKind op of
    Just (ShortCircuitKind PredeterminedForFalse _ _) -> conditionOn (ast a) b
    Just (ShortCircuitKind PredeterminedForTrue _ _)  -> conditionOnNot (ast a) b
    Nothing                                           -> return b
  result <- combine2 (z3BinaryOperator op) a b'
  case proofObligation op of
    Just (failureType, proofObl) -> do
      proofObl' <- proofObl (ast a) (ast b')
      addProofObligation proofObl' failureType p result
    Nothing                      -> return result
