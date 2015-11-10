{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Sara.Z3.SymbolicExecutor ( symbolicExecuteDecl
                                , symbolicExecute
                                , symbolicExecutePure ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Sara.PrettyPrinter
import Sara.Meta
import Sara.Types ( Type (..) )
import Sara.Z3.Utils
import qualified Sara.Z3.ProofPart as P
import Sara.Z3.SymbolicStateSpace
import Sara.Z3.PureExpression
import Sara.Z3.CondAst ( runCondAst )
import qualified Sara.Z3.SymbolicState as St
import qualified Sara.Errors as E
import qualified Sara.Syntax as Sy
import Sara.Z3.Operators
import Sara.Operators
import Z3.Monad

-- | Generate all the proof parts for a declaration.
symbolicExecuteDecl :: (MonadWriter [P.ProofPart] m, MonadZ3 m) => PureCheckerDeclaration -> m ()
symbolicExecuteDecl decl = runStateT (symbolicExecuteDecl' decl) (entry $ declarationPos decl) >> return ()

symbolicExecuteDecl' :: (MonadWriter [P.ProofPart] m, MonadState SymbolicStateSpace m, MonadZ3 m) => PureCheckerDeclaration -> m ()
symbolicExecuteDecl' (Sy.Function Sy.Signature{..} body _) = do
  setArgs args
  mapM_ (addAssumption <=< symbolicExecutePure) preconditions
  symbolicExecute body
  mapM_ addPostcondition postconditions
  collapseAndStop
  where addPostcondition cond = addProofObligation E.PostconditionViolation (expressionPos cond) =<< symbolicExecutePure cond
symbolicExecuteDecl' (Sy.Extern Sy.Signature{..} _)        = do
  setArgs args
  mapM_ (addAssumption <=< symbolicExecutePure) preconditions
  -- In case of an extern, we just have to assume the postconditions are correct.
  -- But note that this is not pointless. It still checks that the conditions are consistent.
  mapM_ (addAssumption <=< symbolicExecutePure) postconditions
  collapseAndStop

collapseAndStop :: (MonadWriter [P.ProofPart] m, MonadState SymbolicStateSpace m) => m ()
collapseAndStop = collapse (error "After collapse and stop, there is no start type.")
                  (error "After collapse and stop, there is no start position.")

setArgs :: (MonadState SymbolicStateSpace m, MonadZ3 m) => [PureCheckerTypedVariable] -> m ()
setArgs args = mapM_ setArg args
  where setArg (Sy.TypedVariable name t (m, _)) = assignVar (m, t) =<< setNewTmpVar ["arg", name] t =<< z3Var m t

symbolicExecute :: (MonadWriter [P.ProofPart] m, MonadState SymbolicStateSpace m, MonadZ3 m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecute exp = if expressionPure exp then symbolicExecutePure exp else symbolicExecuteImpure exp

symbolicExecuteImpure :: (MonadWriter [P.ProofPart] m, MonadState SymbolicStateSpace m, MonadZ3 m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecuteImpure (Sy.Boolean b _)                                              = setNewTmpVar ["boolean"] Boolean =<< mkBool b
symbolicExecuteImpure (Sy.Integer n _)                                              = setNewTmpVar ["integer"] Integer =<< mkInteger n
symbolicExecuteImpure (Sy.UnaryOperation op e (Typed t))                            = computeTmp1 [show op] t (z3UnaryOperator op) =<< symbolicExecute e
symbolicExecuteImpure (Sy.BinaryOperation Assign (Sy.Variable _ m _) e (Typed t))   = do
  e' <- symbolicExecute e
  assignVar (m, t) e'
  return e'
symbolicExecuteImpure (Sy.BinaryOperation op@(shortCircuitKind -> Just ShortCircuitKind{..}) l r (Typed t)) = do
  l' <- symbolicExecute l
  let name = show op
  let executionWhenShortCircuit    = shortCircuitExecution ["whenShortCircuit", name] valueWhenPredetermined l'
  let executionWhenNotShortCircuit = shortCircuitExecution ["whenNotShortCircuit", name] valueWhenNotPredetermined =<< symbolicExecute r
  let splitStates = splitStatesOn ["split", name] t l'
  case predeterminedForValue of
    PredeterminedForFalse -> splitStates executionWhenShortCircuit executionWhenNotShortCircuit
    PredeterminedForTrue  -> splitStates executionWhenNotShortCircuit executionWhenShortCircuit
symbolicExecuteImpure (Sy.BinaryOperation op l r (ExpressionMeta t _, NodeMeta p))    = do
  l' <- symbolicExecute l
  r' <- symbolicExecute r
  result <- computeTmp2 [show op] t (z3BinaryOperator op) l' r'
  case proofObligation op of
    Just (failureType, proofObl) -> do
      proofObl' <- computeTmp2 ["proofObligation", show op] Boolean proofObl l' r'
      addProofObligation failureType p proofObl'
    Nothing                      -> return ()
  return result
symbolicExecuteImpure (Sy.Call name args m (ExpressionMeta t _, NodeMeta p))          = do
  args' <- mapM symbolicExecute args
  let aTyps = map expressionTyp args
  (funcPre, funcPost, func) <- z3FuncDecls m aTyps t
  pre <- computeTmpN ["preconditionApp", name] Boolean (mkApp funcPre) args'
  post <- computeTmpN ["postconditionApp", name] Boolean (mkApp funcPost) args'
  let pure = funcSymPure m
  let failureType = E.PreconditionViolation $ E.functionOrMethod pure name
  addProofObligation failureType p pre
  addAssumption post
  case pure of
    True  -> computeTmpN ["funcApp", name] t (mkApp func) args'
    False -> newTmpVar ["methodResult", name] t
symbolicExecuteImpure (Sy.Variable _ m (Typed t))                                   = return (m, t)
symbolicExecuteImpure (Sy.Conditional cond thenExp elseExp (Typed t))               = do
  cond' <- symbolicExecute cond
  splitStatesOn ["split", "if"] t cond' (symbolicExecute thenExp) (symbolicExecute elseExp)
symbolicExecuteImpure (Sy.While invs cond body (_, NodeMeta p))                     = do
  -- Prove the invariant upon entry.
  mapM (addProofObligation E.LoopEntryInvariantViolation p <=< symbolicExecutePure) invs
  collapse AfterLoopEntry p

  -- For the body execution, assume the invariants
  mapM (addAssumption <=< symbolicExecutePure) invs
  -- Execute and assume the condition.
  cond' <- symbolicExecute cond
  addAssumption cond'
  -- Execute the body
  symbolicExecute body
  -- Prove the invariant preservation
  mapM (addProofObligation E.LoopExitInvariantViolation p <=< symbolicExecutePure) invs
  collapse AfterLoopExit p

  -- Assume the invariant after the loop.
  mapM (addAssumption <=< symbolicExecutePure) invs
  -- Execute and assume the negation of the condition.
  notCond' <- computeTmp1 ["negated", "while", "condition"] Boolean mkNot =<< symbolicExecute cond
  addAssumption notCond'
  return $ error "A while loop doesn't have a return value."
symbolicExecuteImpure (Sy.Assertion Sy.Assume cond _)                               = do
  addAssumption =<< symbolicExecutePure cond
  return $ error "An assert doesn't have a return value."
symbolicExecuteImpure (Sy.Assertion Sy.Assert cond (_, NodeMeta p))                 = do
  addProofObligation E.AssertionViolation p =<< symbolicExecutePure cond
  return $ error "An assume doesn't have a return value."
symbolicExecuteImpure (Sy.Assertion Sy.AssertAndCollapse cond (_, NodeMeta p))      = do
  addProofObligation E.AssertionViolation p =<< symbolicExecutePure cond
  collapse AfterAssertAndCollapse p
  addAssumption =<< symbolicExecutePure cond
  return $ error "An assertAndCollapse doesn't have a return value."
symbolicExecuteImpure (Sy.Block stmts exp _)                                        = do
  mapM_ symbolicExecute stmts
  symbolicExecute exp
symbolicExecuteImpure exp                                                           = error $ "Unsupported expression for verifier: " ++ prettyRender exp

symbolicExecutePure :: (MonadState SymbolicStateSpace m, MonadZ3 m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecutePure exp = computeToTmpVar ["pure"] (expressionTyp exp) $ do
  vars <- gets St.variableStates
  (obl, ass, ast) <- runCondAst <$> runReaderT (z3Expression exp) vars
  St.addProofObligation obl
  St.addAssumption ass
  return ast
  
shortCircuitExecution :: (MonadState SymbolicStateSpace m, MonadZ3 m, MaybeNegation a) => [String] -> a -> ResultTmpVar -> m ResultTmpVar
shortCircuitExecution nameComponents a e | isPositive a = return e
                                         | otherwise    = computeTmp1 ("negate" : nameComponents) Boolean mkNot e

