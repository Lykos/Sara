{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Sara.Z3.SymbolicExecutor ( symbolicExecuteDecl
                                , symbolicExecute
                                , symbolicExecutePure ) where

import Control.Monad.State.Strict
import Control.Monad.Writer
import Sara.PrettyPrinter
import Sara.Meta
import Sara.Types ( Type (..) )
import qualified Sara.Z3.ProofPart as P
import qualified Sara.Z3.Ast as A
import Sara.Z3.SymbolicStateSpace
import Sara.Z3.PureExpression
import Sara.Z3.CondAst ( runCondAst )
import qualified Sara.Z3.SymbolicState as St
import qualified Sara.Errors as E
import qualified Sara.Syntax as Sy
import qualified Sara.Builtins as B
import Sara.Z3.Operators
import Sara.Ast.Operators

-- | Generate all the proof parts for a declaration.
symbolicExecuteDecl :: MonadWriter [P.ProofPart] m => PureCheckerDeclaration -> m ()
symbolicExecuteDecl decl = runStateT (symbolicExecuteDecl' decl) (entry $ declarationPos decl) >> return ()

symbolicExecuteDecl' :: (MonadWriter [P.ProofPart] m, MonadState SymbolicStateSpace m) => PureCheckerDeclaration -> m ()
symbolicExecuteDecl' (Sy.Function Sy.Signature{..} body _) = do
  setArgs args
  mapM_ (addAssumption <=< symbolicExecutePure) preconditions
  let varExpMeta = ExpressionMeta retType True
  let varMeta = BuiltinVar retType B.Result
  let resultVar = Sy.Variable (B.name B.Result) varMeta varExpMeta sigNodeMeta
  let expMeta = ExpressionMeta retType isPure
  let body' = Sy.BinaryOperation Assign resultVar body expMeta sigNodeMeta
  symbolicExecute body'
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

setArgs :: MonadState SymbolicStateSpace m => [PureCheckerTypedVariable] -> m ()
setArgs args = mapM_ setArg args
  where setArg (Sy.TypedVariable name t m _) = assignVar m =<< setNewTmpVar ["arg", name] t (A.Var m)

symbolicExecute :: (MonadWriter [P.ProofPart] m, MonadState SymbolicStateSpace m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecute exp = if expressionPure exp then symbolicExecutePure exp else symbolicExecuteImpure exp

symbolicExecuteImpure :: (MonadWriter [P.ProofPart] m, MonadState SymbolicStateSpace m) => PureCheckerExpression -> m ResultTmpVar
symbolicExecuteImpure (Sy.Boolean b _ _)                                             = setNewTmpVar ["boolean"] Boolean $ A.BoolConst b
symbolicExecuteImpure (Sy.Integer n _ _)                                             = setNewTmpVar ["integer"] Integer $ A.IntConst n
symbolicExecuteImpure (Sy.UnaryOperation op e (ExpressionMeta t _) _)                = computeTmp1 [show op] t (translateUnOp op) =<< symbolicExecute e
symbolicExecuteImpure (Sy.BinaryOperation Assign (Sy.Variable _ m _ _) e _ _)        = do
  e' <- symbolicExecute e
  assignVar m e'
  return e'
symbolicExecuteImpure b@(Sy.BinaryOperation op@(shortCircuitKind -> Just ShortCircuitKind{..}) l r _ _) = do
  let t = expressionTyp b
  l' <- symbolicExecute l
  let name = show op
  let executionWhenShortCircuit    = shortCircuitExecution ["whenShortCircuit", name] valueWhenPredetermined l'
  let executionWhenNotShortCircuit = shortCircuitExecution ["whenNotShortCircuit", name] valueWhenNotPredetermined =<< symbolicExecute r
  let splitStates = splitStatesOn ["split", name] t l'
  case predeterminedForValue of
    PredeterminedForFalse -> splitStates executionWhenShortCircuit executionWhenNotShortCircuit
    PredeterminedForTrue  -> splitStates executionWhenNotShortCircuit executionWhenShortCircuit
symbolicExecuteImpure (Sy.BinaryOperation op l r (ExpressionMeta t _) (NodeMeta p))  = do
  l' <- symbolicExecute l
  r' <- symbolicExecute r
  result <- computeTmp2 [show op] t (translateBinOp op) l' r'
  case proofObligation op of
    Just (failureType, proofObl) -> do
      proofObl' <- computeTmp2 ["proofObligation", show op] Boolean proofObl l' r'
      addProofObligation failureType p proofObl'
    Nothing                      -> return ()
  return result
symbolicExecuteImpure (Sy.Call name args m _ (NodeMeta p))                           = do
  args' <- mapM symbolicExecute args
  let isPure = funcSymPure m
  let t = funcSymRetType m
  result <- case isPure of
    True  -> computeTmpN ["funcApp", name] t (A.App $ A.AppMeta A.FuncApp m) args'
    False -> newTmpVar ["methodResult", name] t
  pre <- computeTmpN ["preconditionApp", name] Boolean (A.App $ A.AppMeta A.PreApp m) args'
  post <- computeTmpN ["postconditionApp", name] Boolean (A.App $ A.AppMeta A.PostApp m) (result : args')
  let failureType = E.PreconditionViolation $ E.functionOrMethod isPure name
  addProofObligation failureType p pre
  addAssumption post
  return result
symbolicExecuteImpure (Sy.Variable _ m _ _)                                          = return m
symbolicExecuteImpure (Sy.Conditional cond thenExp elseExp (ExpressionMeta t _) _)   = do
  cond' <- symbolicExecute cond
  splitStatesOn ["split", "if"] t cond' (symbolicExecute thenExp) (symbolicExecute elseExp)
symbolicExecuteImpure (Sy.While invs cond body _ (NodeMeta p))                       = do
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
  -- Execute and assume the negation of the condition. Note that we cannot reuse cond'.
  cond'' <- symbolicExecute cond
  notCond' <- computeTmp1 ["negated", "while", "condition"] Boolean (A.UnOp A.Not) cond''
  addAssumption notCond'
  return $ error "A while loop doesn't have a return value."
symbolicExecuteImpure (Sy.Assertion Sy.Assume cond _ _)                               = do
  addAssumption =<< symbolicExecutePure cond
  return $ error "An assert doesn't have a return value."
symbolicExecuteImpure (Sy.Assertion Sy.Assert cond _ (NodeMeta p))                    = do
  addProofObligation E.AssertionViolation p =<< symbolicExecutePure cond
  return $ error "An assume doesn't have a return value."
symbolicExecuteImpure (Sy.Assertion Sy.AssertAndCollapse cond _ (NodeMeta p))         = do
  addProofObligation E.AssertionViolation p =<< symbolicExecutePure cond
  collapse AfterAssertAndCollapse p
  addAssumption =<< symbolicExecutePure cond
  return $ error "An assertAndCollapse doesn't have a return value."
symbolicExecuteImpure (Sy.Block stmts exp _ _)                                        = do
  mapM_ symbolicExecute stmts
  symbolicExecute exp
symbolicExecuteImpure exp                                                             = error $ "Unsupported expression for verifier: " ++ prettyRender exp

symbolicExecutePure :: MonadState SymbolicStateSpace m => PureCheckerExpression -> m ResultTmpVar
symbolicExecutePure exp = computeToTmpVar ["pure"] (expressionTyp exp) $ do
  (obl, ass, ast) <- runCondAst <$> translateExpression exp
  St.addProofObligation obl
  St.addAssumption ass
  return ast
  
shortCircuitExecution :: (MonadState SymbolicStateSpace m, MaybeNegation a) => [String] -> a -> ResultTmpVar -> m ResultTmpVar
shortCircuitExecution nameComponents a e | isPositive a = return e
                                         | otherwise    = computeTmp1 ("negate" : nameComponents) Boolean (A.UnOp A.Not) e

