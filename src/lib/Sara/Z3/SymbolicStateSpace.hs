module Sara.Z3.SymbolicStateSpace where

import Sara.SymbolicState

data SymbolicStateSpace = [SymbolicState]

-- | Adds the first argument as the proof obligation to all states in the second argument.
addProofObligation :: MonadZ3 z3 => AST -> VerifierFailureType -> SourcePos -> SymbolicStateSpace -> z3 SymbolicStateSpace
addProofObligation newObl failureType pos SymbolicStateSpace{ states } = SymbolicStateSpace <$> mapM addProofObligation 

-- | Adds the first argument as the assumption to all states in the second argument.
addAssumption :: MonadZ3 z3 => AST -> SymbolicState -> z3 SymbolicState
addProofObligation newObl failureType pos SymbolicStateSpace{ states } = SymbolicStateSpace <$> mapM addProofObligation 
