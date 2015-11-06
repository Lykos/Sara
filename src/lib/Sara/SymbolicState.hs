{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | Helper functions to be used by the verifier to transform our AST into a Z3 AST.
module Sara.SymbolicState ( SymbolicExecutionStart
                          , SymbolicState
                          , empty
                          , getVar
                          , setVar
                          , getOrCreateVar
                          , addProofObligation
                          , addAssumption ) where

import qualified Data.Map as M
import Z3.Monad
import Sara.Errors (VerifierFailureType)
import Sara.Z3Utils
import Sara.Types
import qualified Sara.AstWrapper as W
import Sara.Meta
import Text.Parsec.Pos

data SymbolicExecutionStart
  = MethodEntry
  | LoopEntry
  | AssertAndCollapse
  deriving (Eq, Ord, Show, Enum, Bounded)

data SymbolicState
  = SymbolicState { startPos :: SourcePos
                  , startType :: SymbolicExecutionStart
                  , variableInitialStates :: M.Map VariableMeta AST
                  , variableStates :: M.Map VariableMeta AST
                  , proofObligation :: W.ProofObligation
                  , assumption :: W.Assumption }
  deriving (Eq, Ord, Show)

-- | An empty symbolic state with no variable states, assumptions and proof obligation.
empty :: SourcePos -> SymbolicExecutionStart -> SymbolicState
empty pos start = SymbolicState pos start M.empty M.empty W.empty W.empty

-- | Sets the variable to the given value. If this variable is not set yet, then this will also be set as the initial value.
setVar :: VariableMeta -> AST -> SymbolicState -> SymbolicState
setVar v a s@SymbolicState{..} = let insertVar = M.insert v a
                                     s' = s{ variableStates = insertVar variableStates }
                                 in case getVar v s of
  Just _  -> s'
  Nothing -> s'{ variableInitialStates = insertVar variableInitialStates }

getOrCreateVar :: MonadZ3 z3 => VariableMeta -> Type -> SymbolicState -> z3 (SymbolicState, AST)
getOrCreateVar v t s@SymbolicState{..} = case getVar v s of
  Just ast -> return (s, ast)
  Nothing  -> do
    ast <- mkFreshVar (havocVarName v s) =<< z3Sort t
    let s' = setVar v ast s
    return (s', ast)
  where havocVarName (VariableMeta name index) SymbolicState{..} =
          z3VarName [ "havocedVar", show startType
                    , sourceName startPos, show $ sourceLine startPos, show $ sourceColumn startPos
                    , name, show index]

getVar :: VariableMeta -> SymbolicState -> Maybe AST
getVar v SymbolicState{..} = M.lookup v variableStates

-- | Adds the first argument as the proof obligation to the second argument.
addProofObligation :: MonadZ3 z3 => AST -> VerifierFailureType -> SourcePos -> SymbolicState -> z3 SymbolicState
addProofObligation newObl failureType pos s@SymbolicState{ proofObligation = obl } =
  return $ s{ proofObligation = W.conjunct [newObl', obl] }
  where newObl' = W.singleton newObl (failureType, pos)

-- | Adds the first argument as the assumption to the second argument.
addAssumption :: MonadZ3 z3 => AST -> SymbolicState -> z3 SymbolicState
addAssumption newAss s@SymbolicState{ assumption = ass } = return $ s{ assumption = W.conjunct [newAss', ass] }
  where newAss' = W.singleton newAss ()
