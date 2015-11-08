module Sara.Z3.ProofPart ( ProofPart(..) ) where

import Sara.Z3.AstWrapper

data ProofPart = ProofPart Assumption ProofObligation
