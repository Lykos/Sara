{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Representation of one single symbolic execution state.
module Sara.Z3.SymbolicState ( SymbolicState
                             , E.SymbolicExecutionStart (..)
                             , variableStates
                             , toProofPart
                             , empty
                             , getVar
                             , setVar
                             , getOrCreateVar
                             , addProofObligation
                             , addAssumption ) where

import Control.Monad.State
import qualified Data.Map as M
import Z3.Monad
import Sara.Z3.Utils
import qualified Sara.Z3.ProofPart as P
import Sara.Types
import qualified Sara.Errors as E
import qualified Sara.Z3.AstWrapper as W
import Sara.Meta
import Text.Parsec.Pos

data SymbolicState
  = SymbolicState { startPos :: SourcePos
                  , startType :: E.SymbolicExecutionStart
                  , variableInitialStates :: M.Map VariableMeta AST
                  , variableStates :: M.Map VariableMeta AST
                  , proofObligation :: W.ProofObligation
                  , assumption :: W.Assumption }
  deriving (Eq, Ord, Show)

toProofPart :: SymbolicState -> P.ProofPart
toProofPart SymbolicState{..} = P.ProofPart startPos startType variableInitialStates assumption proofObligation

-- | An empty symbolic state with no variable states, assumptions and proof obligation.
empty :: SourcePos -> E.SymbolicExecutionStart -> SymbolicState
empty pos entryType = SymbolicState pos entryType M.empty M.empty W.empty W.empty

-- | Sets the variable to the given value. If this variable is not set yet, then this will also be set as the initial value.
setVar :: MonadState SymbolicState m => VariableMeta -> AST -> m ()
setVar v a = do
  v' <- getVar v
  let insertVar = M.insert v a
  modify $ \s@SymbolicState{..} -> s{ variableStates = insertVar variableStates }
  case v' of
    Just _  -> return ()
    Nothing -> modify $ \s@SymbolicState{..} -> s{ variableInitialStates = insertVar variableInitialStates }

-- | If the variable exists, returns its value, otherwise, creates a temporary Z3 variable and returns it.
getOrCreateVar :: (MonadState SymbolicState m, MonadZ3 m) => VariableMeta -> Type -> m AST
getOrCreateVar v t = do
  v' <- getVar v
  case v' of
    Just ast -> return ast
    Nothing  -> do
      name <- gets $ havocVarName v
      ast <- mkFreshVar name =<< z3Sort t
      setVar v ast
      return ast
  where havocVarName (VariableMeta name index) SymbolicState{..} =
          z3VarName [ "havocedVar", show startType
                    , sourceName startPos, show $ sourceLine startPos, show $ sourceColumn startPos
                    , name, show index]

getVar :: MonadState SymbolicState m => VariableMeta -> m (Maybe AST)
getVar v = gets $ M.lookup v . variableStates

-- | Adds the first argument as the proof obligation to the second argument.
addProofObligation :: MonadState SymbolicState m => W.ProofObligation -> m ()
addProofObligation newObl = modify $ \s@SymbolicState{ proofObligation = obl } -> s{ proofObligation = W.conjunct [newObl, obl] }

-- | Adds the first argument as the assumption to the second argument.
addAssumption :: MonadState SymbolicState m => W.Assumption -> m ()
addAssumption newAss = modify $ \s@SymbolicState{ assumption = ass } -> s{ assumption = W.conjunct [newAss, ass] }
