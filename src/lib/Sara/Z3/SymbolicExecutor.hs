{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Sara.Z3.SymbolicExecutor where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Sara.Meta
import Sara.Types ( Type (..) )
import Sara.Z3.Utils
import Sara.Z3.SymbolicStateSpace
import Sara.Z3.PureExpression
import Sara.Z3.CondAst ( runCondAst )
import qualified Sara.Z3.SymbolicState as S
import qualified Sara.Errors as E
import qualified Sara.Syntax as S
import Sara.Z3.Operators
import Sara.Operators
import Z3.Monad

shortCircuitExecution :: (MonadState SymbolicStateSpace m, MonadZ3 m, MaybeNegation a) => [String] -> a -> ResultTmpVar -> m ResultTmpVar
shortCircuitExecution nameComponents a e | isPositive a = return e
                                         | otherwise    = computeTmp1 ("negate" : nameComponents) Boolean mkNot e

symbolicExecute :: (MonadState SymbolicStateSpace m, MonadZ3 m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecute exp = if expressionPure exp then symbolicExecutePure exp else symbolicExecuteImpure exp

symbolicExecuteImpure :: (MonadState SymbolicStateSpace m, MonadZ3 m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecuteImpure (S.Boolean b _)                                              = setNewTmpVar ["boolean"] Boolean =<< mkBool b
symbolicExecuteImpure (S.Integer n _)                                              = setNewTmpVar ["integer"] Integer =<< mkInteger n
symbolicExecuteImpure (S.UnaryOperation op e (Typed t))                            = computeTmp1 [show op] t (z3UnaryOperator op) =<< symbolicExecute e
symbolicExecuteImpure (S.BinaryOperation Assign (S.Variable _ m _) e (Typed t))    = do
  e' <- symbolicExecute e
  assignVar (m, t) e'
  return e'
symbolicExecuteImpure (S.BinaryOperation op@(shortCircuitKind -> Just ShortCircuitKind{..}) l r (Typed t)) = do
  l' <- symbolicExecute l
  let name = show op
  let executionWhenShortCircuit    = shortCircuitExecution ["whenShortCircuit", name] valueWhenPredetermined l'
  let executionWhenNotShortCircuit = shortCircuitExecution ["whenNotShortCircuit", name] valueWhenNotPredetermined =<< symbolicExecute r
  let splitStates = splitStatesOn ["split", name] t l'
  case predeterminedForValue of
    PredeterminedForFalse -> splitStates executionWhenShortCircuit executionWhenNotShortCircuit
    PredeterminedForTrue  -> splitStates executionWhenNotShortCircuit executionWhenShortCircuit
symbolicExecuteImpure (S.BinaryOperation op l r (ExpressionMeta t, NodeMeta p))    = do
  l' <- symbolicExecute l
  r' <- symbolicExecute r
  result <- computeTmp2 [show op] t (z3BinaryOperator op) l' r'
  case proofObligation op of
    Just (failureType, proofObl) -> do
      proofObl' <- computeTmp2 ["proofObligation", show op] Boolean proofObl l' r'
      addProofObligation proofObl' failureType p
    Nothing                      -> return ()
  return result
symbolicExecuteImpure (S.Call name args m (ExpressionMeta t, NodeMeta p))          = do
  args' <- mapM symbolicExecute args
  let aTyps = map expressionTyp args
  (funcPre, funcPost, func) <- z3FuncDecls m aTyps t
  pre <- computeTmpN ["preconditionApp", name] Boolean (mkApp funcPre) args'
  post <- computeTmpN ["postconditionApp", name] Boolean (mkApp funcPost) args'
  let pure = funcSymPure m
  let failureType = E.PreconditionViolation $ E.functionOrMethod pure name
  addProofObligation pre failureType p
  addAssumption post
  case pure of
    True  -> computeTmpN ["funcApp", name] t (mkApp func) args'
    False -> newTmpVar ["methodResult", name] t
symbolicExecuteImpure (S.Variable _ m (Typed t))                                   = return (m, t)
symbolicExecuteImpure (S.Conditional cond thenExp elseExp (Typed t))               = do
  cond' <- symbolicExecute cond
  splitStatesOn ["split", "if"] t cond' (symbolicExecute thenExp) (symbolicExecute elseExp)
symbolicExecuteImpure (S.While invs cond body (_, NodeMeta p))                     = do
  invs' <- ["loop", "entry", "invariant"] Boolean
  mapM (addProofObligation E.LoopEntryInvariantViolation p) invs
  collapse LoopEntry p
  mapM addAssumption invs
  cond' <- symbolicExecute cond
  addAssumption cond
  symbolicExecute body
  mapM (addProofObligation E.LoopExitInvariantViolation p) invs
  collapse LoopExit p
  return $ error "A while loop doesn't have a return value."

symbolicExecutePure :: (MonadState SymbolicStateSpace m, MonadZ3 m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecutePure exp = computeToTmpVar $ do
  vars <- gets S.variableStates
  (obl, ass, ast) <- runCondAst <$> runReaderT (z3Expression exp) vars
  S.addProofObligation obl
  S.addAssumption ass
  return ast
  
