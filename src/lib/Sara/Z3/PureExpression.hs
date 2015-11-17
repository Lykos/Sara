{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Sara.Z3.PureExpression ( translateExpression ) where

import Control.Monad.State.Strict
import Sara.Parser.PrettyPrinter
import Sara.Errors as E
import Sara.Z3.CondAst
import qualified Sara.Z3.Ast as A
import qualified Sara.Z3.Operators as O
import Sara.Ast.Operators
import qualified Sara.Z3.SymbolicState as S
import qualified Sara.Ast.Syntax as S
import Sara.Ast.Meta
import Text.Parsec.Pos (SourcePos)

-- | Create an expression and its precondition and postcondition.
translateExpression :: (MonadState S.SymbolicState m) => PureCheckerExpression -> m CondAst
translateExpression (S.Boolean b _ _)                = return $ trivial $ A.BoolConst b
translateExpression (S.Integer n _ _)                = return $ trivial $ A.IntConst n
translateExpression (S.UnaryOperation op e _ _)      = do
  e' <- translateExpression e
  return $ translateUnOp op e'
translateExpression b@(S.BinaryOperation op l r _ _) = do
  l' <- translateExpression l
  r' <- translateExpression r
  -- TODO short circuit
  let pos = expressionPos b
  return $ translateBinOp pos op l' r'
translateExpression (S.Variable _ m _ _)             = trivial <$> S.getOrCreateVar m
translateExpression (S.Conditional c t e _ _)        = do
  c' <- translateExpression c
  t' <- conditionOn (ast c') <$> translateExpression t
  e' <- conditionOnNot (ast c') <$> translateExpression e
  return $ combine3 A.Ite c' t' e'
translateExpression c@(S.Call name a m _ _)          = do
  a' <- mapM translateExpression a
  let results = combineN (A.App (A.AppMeta A.FuncApp m)) a'
  let result = ast results
  let args = map ast a'
  let pre = A.App (A.AppMeta A.PreApp m) args
  let post = A.App (A.AppMeta A.PostApp m) (result : args)
  let pos = expressionPos c
  let failureType = PreconditionViolation (E.Function name)
  return $ addProofObligation pre failureType pos $ addAssumption post results
translateExpression exp                            = error $ "Unsupported expression for pure verifier: " ++ prettyRender exp

translateUnOp :: UnaryOperator -> CondAst -> CondAst
translateUnOp op = liftCond $ O.translateUnOp op

translateBinOp :: SourcePos -> BinaryOperator -> CondAst -> CondAst -> CondAst
translateBinOp p op a b         = case O.proofObligation op of
  Just (failureType, proofObl) -> let proofObl' = proofObl (ast a) (ast b') in
    addProofObligation proofObl' failureType p resultWithoutProofObl
  Nothing                      -> resultWithoutProofObl
  where b' = case shortCircuitKind op of
          Just (ShortCircuitKind PredeterminedForFalse _ _) -> conditionOn (ast a) b
          Just (ShortCircuitKind PredeterminedForTrue _ _)  -> conditionOnNot (ast a) b
          Nothing                                           -> b
        resultWithoutProofObl = combine2 (O.translateBinOp op) a b'
