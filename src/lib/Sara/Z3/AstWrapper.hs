{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Data structures to keep track of some properties of the Z3 AST structure.
module Sara.Z3.AstWrapper ( ProofObligation
                          , Assumption
                          , FailureTrackableAST
                          , empty
                          , singleton
                          , conjunct
                          , conditionOn
                          , runAst
                          , runProofObligation
                          , findFailure ) where

import Sara.Z3.Utils
import Data.Maybe
import Sara.Errors (VerifierFailureType)
import Text.Parsec.Pos (SourcePos)
import Z3.Monad

-- | AST structure container for which a failure can be tracked to the failing part.
data TrackableAST a
  = Singleton AST a
  | Conjunction [TrackableAST a]
  | Condition AST (TrackableAST a)
  deriving (Eq, Ord, Show)

pattern Empty = Conjunction []

-- | AST structure container for which a failure can be tracked.
type FailureTrackableAST = TrackableAST (VerifierFailureType, SourcePos)

class ASTWrapper a b | a -> b where
  unwrap :: a -> TrackableAST b
  wrap :: TrackableAST b -> a

-- | AST Wrapper for proof obligations.
-- We use this to store additional information with the AST and to ensure better type safety.
newtype ProofObligation
  = ProofObligation { unObl :: FailureTrackableAST }
  deriving (Eq, Ord, Show)

-- | AST Wrapper for assumptions.
-- We use this to for better type safety.
newtype Assumption
  = Assumption { unAss :: TrackableAST () }
  deriving (Eq, Ord, Show)

instance ASTWrapper ProofObligation (VerifierFailureType, SourcePos) where
  wrap = ProofObligation
  unwrap = unObl

instance ASTWrapper Assumption () where
  wrap = Assumption
  unwrap = unAss

instance ASTWrapper (TrackableAST b) b where
  unwrap = id
  wrap = id

empty :: ASTWrapper a b => a
empty = wrap Empty

singleton :: ASTWrapper a b => AST -> b -> a
singleton ast = wrap . Singleton ast

conjunct :: ASTWrapper a b => [a] -> a
conjunct as = wrap $ Conjunction $ concatMap extract $ map unwrap $ as
  where extract (Conjunction as) = as
        extract a                = [a]

conditionOn :: ASTWrapper a b => AST -> a -> a
conditionOn ast = wrap . conditionOn' . unwrap
  where conditionOn' Empty = Empty
        conditionOn' a     = Condition ast a

runAst :: (ASTWrapper a b, MonadZ3 z3) => a -> z3 AST
runAst a = runAst' (unwrap a)
  where runAst' Empty             = mkTrue
        runAst' (Conjunction [a]) = runAst' a
        runAst' (Conjunction as)  = conjunctAsts =<< mapM runAst' as
        runAst' (Singleton ast _) = return ast
        runAst' (Condition ast a) = mkImplies ast =<< runAst' a

runProofObligation :: ProofObligation -> FailureTrackableAST
runProofObligation = unwrap

findFailure :: MonadZ3 z3 => Model -> TrackableAST a -> z3 (Maybe a)
findFailure model (Singleton ast result) = do
  val <- evalBool model ast
  case val of
    Nothing    -> return Nothing
    Just True  -> return Nothing
    Just False -> return $ Just result
findFailure model (Conjunction subs)     = do
  subFailures <- mapM (findFailure model) subs
  return $ listToMaybe $ catMaybes $ subFailures
findFailure model (Condition cond sub)   = do
  val <- evalBool model cond
  case val of
    Nothing    -> return Nothing
    Just True  -> findFailure model sub
    Just False -> return Nothing
