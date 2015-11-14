{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Sara.Z3.SymbolicStateSpace ( SymbolicStateSpace
                                  , S.SymbolicExecutionStart (..)
                                  , ResultTmpVar
                                  , newTmpVar
                                  , setNewTmpVar
                                  , splitStatesOn
                                  , assignVar
                                  , computeToTmpVar
                                  , computeTmp1
                                  , computeTmp2
                                  , computeTmpN
                                  , entry
                                  , collapse
                                  , addAssumption
                                  , addProofObligation ) where

import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Sara.Z3.Ast as A
import qualified Sara.Z3.AstWrapper as W
import Sara.Z3.ProofPart
import Sara.Z3.Utils
import Sara.Syntax ( Name )
import Sara.Errors ( VerifierFailureType )
import qualified Sara.Z3.SymbolicState as S
import Sara.Types ( Type( Boolean ) )
import Sara.Meta
import Text.Parsec.Pos

type ResultTmpVar = VariableMeta

-- | We intentionally use a name with multiple components to ensure that there are no clashes with user variables.
tmpName :: [String] -> Name
tmpName nameComponents = z3VarName $ "tmp" : nameComponents

data SymbolicStateSpace = SymbolicStateSpace { tmpIndex :: Id, states :: [S.SymbolicState] }

-- | Models a nondeterministic choice between two possible executions based on whether a given temporary variable is true.
splitStatesOn :: (MonadState SymbolicStateSpace m) =>
                 [String] -> Type -> ResultTmpVar -> m ResultTmpVar -> m ResultTmpVar -> m ResultTmpVar
splitStatesOn nameComponents typ cond f g = do
  notCond <- computeTmp1 ("notSplitCond" : nameComponents) Boolean (A.UnOp A.Not) cond
  let f' = addAssumption cond >> f
  let g' = addAssumption notCond >> g
  splitStates nameComponents typ f' g'

-- | Models a nondeterministic choice between two possible executions.
splitStates :: (MonadState SymbolicStateSpace m) =>
               [String] -> Type -> m ResultTmpVar -> m ResultTmpVar -> m ResultTmpVar
splitStates nameComponents typ f g = do
  resultVar <- newTmpVar ("splitStates" : nameComponents) typ
  oldStates <- gets states
  assignVar resultVar =<< f
  statesAfterF <- gets states
  modify $ \s -> s{ states = oldStates }
  assignVar resultVar =<< g
  statesAfterG <- gets states
  modify $ \s -> s{ states = statesAfterF ++ statesAfterG }
  return resultVar

-- | Create a new temporary variable.
newTmpVar :: MonadState SymbolicStateSpace m => [String] -> Type -> m ResultTmpVar
newTmpVar nameComponents typ = do
  idx <- gets tmpIndex
  modify $ \s@SymbolicStateSpace{ tmpIndex = idx } -> s{ tmpIndex = idx + 1 }
  return (VariableMeta typ (tmpName nameComponents) idx)

-- | Create a new temporary variable, set it to a given value in all states and return the variable.
setNewTmpVar :: (MonadState SymbolicStateSpace m) => [String] -> Type -> A.Ast -> m ResultTmpVar
setNewTmpVar nameComponents typ ast = computeToTmpVar nameComponents typ $ return ast

-- | Modify the states by applying the given transformation to every state.
liftStates :: (MonadState SymbolicStateSpace m)
              => (forall n . MonadState S.SymbolicState n => n ())
              -> m ()
liftStates transform = do
  SymbolicStateSpace id states <- get
  states' <- mapM (execStateT transform) states
  put $ SymbolicStateSpace id states'

-- | Creates a temporary variable, hands it to the transformation function for every state and returns it.
computeToTmpVar :: (MonadState SymbolicStateSpace m)
              => [String] -> Type
              -> (forall n . MonadState S.SymbolicState n => n A.Ast)
              -> m ResultTmpVar
computeToTmpVar nameComponents typ transform = do
  resultVar <- newTmpVar nameComponents typ
  -- TODO Don't set initialState for the variables here!
  liftStates $ S.setVar resultVar =<< transform
  return resultVar

-- | In all states, load the given temporary variable, apply the given transformation, store it into another temporary variable and return that variable.
computeTmp1 :: (MonadState SymbolicStateSpace m)
               => [String] -> Type
               -> (A.Ast -> A.Ast)
               -> ResultTmpVar -> m ResultTmpVar
computeTmp1 nameComponents typ transform1 input = computeToTmpVar nameComponents typ $ do
  input' <- getOrCreateVar input
  return $ transform1 input'

-- | In all states, load the given temporary variables, apply the given transformation, store it into another temporary variable and return that variable.
computeTmp2 :: MonadState SymbolicStateSpace m
               => [String] -> Type
               -> (A.Ast -> A.Ast -> A.Ast)
               -> ResultTmpVar -> ResultTmpVar -> m ResultTmpVar
computeTmp2 nameComponents typ transform2 left right = computeToTmpVar nameComponents typ $ do
  left' <- getOrCreateVar left
  right' <- getOrCreateVar right
  return $ transform2 left' right'

-- | In all states, load the given temporary variables, apply the given transformation, store it into another temporary variable and return that variable.
computeTmpN :: MonadState SymbolicStateSpace m
               => [String] -> Type
               -> ([A.Ast] -> A.Ast)
               -> [ResultTmpVar] -> m ResultTmpVar
computeTmpN nameComponents typ transformN inputs = computeToTmpVar nameComponents typ $ do
  inputs' <- mapM getOrCreateVar inputs
  return $ transformN inputs'

-- | In all states, load the given temporary variable, assign it to the given lhs.
assignVar :: MonadState SymbolicStateSpace m => ResultTmpVar -> ResultTmpVar -> m ()
assignVar lhs rhs = liftStates $ do
  ast <- getOrCreateVar rhs
  S.setVar lhs ast

getOrCreateVar :: MonadState S.SymbolicState m => ResultTmpVar -> m A.Ast
getOrCreateVar v = do
  state <- get
  runReaderT (S.getOrCreateVar v) state

-- | Adds the given proof obligation to all states.
addProofObligation :: MonadState SymbolicStateSpace m => VerifierFailureType -> SourcePos -> ResultTmpVar -> m ()
addProofObligation failureType pos newObl = liftStates $ do
  newObl' <- getOrCreateVar newObl
  S.addProofObligation $ W.singleton newObl' (failureType, pos)

-- | Adds the given proof assumption to all states.
addAssumption :: MonadState SymbolicStateSpace m => ResultTmpVar -> m ()
addAssumption newAss = liftStates $ do
  newAss' <- getOrCreateVar newAss
  S.addAssumption $ W.singleton newAss' ()

entry :: SourcePos -> SymbolicStateSpace
entry pos = SymbolicStateSpace 0 [S.empty pos S.AfterMethodEntry]

collapse :: (MonadWriter [ProofPart] m, MonadState SymbolicStateSpace m) => S.SymbolicExecutionStart -> SourcePos -> m ()
collapse entryType pos = do
  SymbolicStateSpace idx states <- get
  mapM (tell . (:[]) . S.toProofPart) states
  put $ SymbolicStateSpace idx [S.empty pos entryType]
