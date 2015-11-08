{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Sara.Z3.SymbolicExecutor where

import Sara.Meta
import Sara.Syntax (Name)
import Sara.Z3.Utils
import qualified Sara.Z3.SymbolicState as S
import qualified Sara.Syntax as S
import Sara.Z3.Operators

type ResultTmpVar = VariableMeta

-- | We intentionally use a name with multiple components to ensure that there are no clashes with user variables.
tmpName :: [String] -> Name
tmpName nameComponents = z3VarName $ ["tmp"] ++ nameComponents

data SymbolicExecutionState = SymbolicExecutionState { tmpIndex :: Id, states :: [S.SymbolicState] }

shortCircuitExecution :: (MonadState SymbolicExecutionState m, MonadZ3 m) => String -> ShortCircuitKind -> ResultTmpVar -> m ResultTmpVar
shortCircuitExecution name ShortCircuitKind{..} l = do
  assumption = case predeterminedForValue of
    PredeterminedForFalse -> computeTmp1 ["notLeftSideAssumption", "shortCircuit", name] mkNot l
    PredeterminedForTrue  -> return l
  addAssumption assumption
  case valueWhenPredetermined of
    LeftSideWhenPredetermined    -> return l
    NotLeftSideWhenPredetermined -> computeTmp1 ["notLeftSideResult", "shortCircuit", name] mkNot r'

notShortCircuitExecution :: (MonadState SymbolicExecutionState m, MonadZ3 m) => String -> ShortCircuitKind -> ResultTmpVar -> SymbolizerExpression -> m ResultTmpVar
notShortCircuitExecution name ShortCircuitKind{..} l r = do
  assumption = case predeterminedForValue of
    PredeterminedForTrue  -> computeTmp1 ["notLeftSideAssumption", "notShortCircuit", name] mkNot l
    PredeterminedForFalse -> return l
  addAssumption assumption
  r' <- symbolicExecute r
  case valueWhenPredetermined of
    RightSideWhenNotPredetermined    -> return r'
    NotRightSideWhenNotPredetermined -> computeTmp1 ["notRightSideResult", "notShortCircuit", name] mkNot r'

symbolicExecute :: (MonadState SymbolicExecutionState m, MonadZ3 m) => SymbolizerExpression -> m ResultTmpVar
symbolicExecute (S.Boolean b _)                                 = computeTmp1 ["boolean"] mkBool b
symbolicExecute (S.Integer n _)                                 = computeTmp1 ["integer"] mkInteger n
symbolicExecute (S.UnaryOperation op e _)                       = computeTmp1 [show op] (z3UnaryOperator op) =<< symbolicExecute e
symbolicExecute (S.BinaryOperation Assign (Variable _ m _) e _) = do
  e' <- symbolicExecute e
  assignVar m e'
  return e'
symbolicExecute (S.BinaryOperation op@(shortCircuitKind -> Just kind) l r _) = do
  l' <- symbolicExecute e
  let name = show op
  splitStates name (shortCircuitExecution name kind l') (notShortCircuitExecution name kind l' r)
symbolicExecute (S.BinaryOperation op l r) = do
  l' <- symbolicExecute l
  r' <- symbolicExecute r
  computeTmp2 [show op] (z3BinaryOperator op) l' r'

newTmpVar :: MonadState SymbolicExecutionState m => [String] -> m ResultTmpVar
newTmpVar nameComponents = do
  modify $ \s@SymbolicExecutionState{ tmpIndex = idx } -> s{ tmpIndex = idx + 1 }
  idx <- gets tmpIndex
  return VariableMeta (tmpName nameComponents) idx

setNewTmpVar :: MonadState SymbolicExecutionState m => [String] -> AST -> m ResultTmpVar
setNewTmpVar nameComponents ast = do
  var <- newTmpVar nameComponents
  modifyStates $ S.setVar var ast
  return var

computeTmp1 :: (MonadState SymbolicExecutionState m, MonadZ3 m) => [String] -> (AST -> m AST) -> ResultTmpVar -> m ResultTmpVar
computeTmp1 nameComponents transform1 input = setNewTmpVar nameComponents =<< transform1 =<< S.getOrCreateVar input

computeTmp2 :: (MonadState SymbolicExecutionState m, MonadZ3 m) => [String] -> (AST -> AST -> m AST) -> ResultTmpVar -> ResultTmpVar -> m ResultTmpVar
computeTmp2 nameComponents transform2 left right = setNewTmpVar nameComponents =<< transform2 =<< S.getOrCreateVar left <*> S.getOrCreateVar right

computeTmpN :: (MonadState SymbolicExecutionState m, MonadZ3 m) => [String] -> ([AST] -> m AST) -> [ResultTmpVar] -> m ResultTmpVar
computeTmpN nameComponents transformN inputs = setNewTmpVar nameComponents =<< transformN =<< mapM S.getOrCreateVar inputs

assignVar :: MonadState SymbolicExecutionState m => ResultTmpVar -> ResultTmpVar -> m ()
assignVar lhs rhs = modifyStates $ S.setVar lhs $ S.getOrCreateVar rhs

-- | Modify the states by applying the given function to every state.
modifyStates :: (MonadState SymbolicExecutionState m) => (SymbolicState -> SymbolicState) -> m ()
modifyStates f = modify $ \s@SymbolicExecutionState{ states = oldStates } -> s{ states = map f oldStates }

-- | Models a nondeterministic choice between two possible executions.
splitStates :: (MonadState SymbolicExecutionState m, MonadZ3 z3) => [String] -> m ResultTmpVar -> m ResultTmpVar -> m ResultTmpVar
splitStates name f g = do
  resultVar <- newTmpVar ["splitStates", name]
  s@(SymbolicExecutionState id oldStates) <- get
  let SymbolicExecutionState idAfterF statesAfterF = execStateT (assignVar resultVar =<< f) s
  let SymbolicExecutionState idAfterG statesAfterG = execStateT (assignVar resultVar =<< g) (SymbolicExecutionState idAfterF oldStates)
  put SymbolicExecutionState idAfterG [statesAfterF, statesAfterG]
  return resultVar
