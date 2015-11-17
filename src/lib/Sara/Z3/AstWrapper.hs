{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Data structures to keep track of some properties of the Ast structure.
module Sara.Z3.AstWrapper ( ProofObligation
                          , Assumption
                          , FailureTrackableAst
                          , empty
                          , singleton
                          , conjunct
                          , conditionOn
                          , runAst
                          , runProofObligation
                          , substituteFuncs
                          , findFailure ) where

import qualified Sara.Z3.Ast as A
import Sara.Z3.CodeGenerator
import Sara.Ast.Meta
import Data.Maybe
import Sara.Errors (VerifierFailureType)
import Text.Parsec.Pos (SourcePos)
import Z3.Monad
import qualified Data.Map as M

-- | Ast structure container for which a failure can be tracked to the failing part.
data TrackableAst a
  = Singleton A.Ast a
  | Conjunction [TrackableAst a]
  | Condition A.Ast (TrackableAst a)
  deriving (Eq, Ord, Show)

pattern Empty = Conjunction []

-- | A.Ast structure container for which a failure can be tracked.
type FailureTrackableAst = TrackableAst (VerifierFailureType, SourcePos)

class AstWrapper a b | a -> b where
  unwrap :: a -> TrackableAst b
  wrap :: TrackableAst b -> a

-- | A.Ast Wrapper for proof obligations.
-- We use this to store additional information with the A.Ast and to ensure better type safety.
newtype ProofObligation
  = ProofObligation { unObl :: FailureTrackableAst }
  deriving (Eq, Ord, Show)

-- | A.Ast Wrapper for assumptions.
-- We use this to for better type safety.
newtype Assumption
  = Assumption { unAss :: TrackableAst () }
  deriving (Eq, Ord, Show)

instance AstWrapper ProofObligation (VerifierFailureType, SourcePos) where
  wrap = ProofObligation
  unwrap = unObl

instance AstWrapper Assumption () where
  wrap = Assumption
  unwrap = unAss

instance AstWrapper (TrackableAst b) b where
  unwrap = id
  wrap = id

empty :: AstWrapper a b => a
empty = wrap Empty

singleton :: AstWrapper a b => A.Ast -> b -> a
singleton ast = wrap . Singleton ast

conjunct :: AstWrapper a b => [a] -> a
conjunct as = wrap $ Conjunction $ concatMap extract $ map unwrap $ as
  where extract (Conjunction as) = as
        extract a                = [a]

conditionOn :: AstWrapper a b => A.Ast -> a -> a
conditionOn ast = wrap . conditionOn' . unwrap
  where conditionOn' Empty = Empty
        conditionOn' a     = Condition ast a

runAst :: AstWrapper a b => a -> A.Ast
runAst a = A.simplify $ runAst' (unwrap a)
  where runAst' (Conjunction as)  = A.NaryOp A.And $ map runAst' as
        runAst' (Singleton ast _) = ast
        runAst' (Condition ast a) = A.BinOp A.Implies ast $ runAst' a

runProofObligation :: ProofObligation -> FailureTrackableAst
runProofObligation = unwrap

substituteFuncs :: AstWrapper a b => M.Map A.AppMeta ([VariableMeta], A.Ast) -> a -> a
substituteFuncs funcs = wrap . substituteFuncs' . unwrap
  where substituteFuncs' (Conjunction as)  = Conjunction $ map substituteFuncs' as
        substituteFuncs' (Singleton ast a) = Singleton (A.substituteFuncs funcs ast) a
        substituteFuncs' (Condition ast a) = Condition (A.substituteFuncs funcs ast) (substituteFuncs' a)

-- TODO Move this somewhere else.
findFailure :: MonadZ3 z3 => Model -> TrackableAst a -> z3 (Maybe a)
findFailure model (Singleton ast result) = do
  ast' <- codegen ast
  val <- evalBool model ast'
  case val of
    Nothing    -> return Nothing
    Just True  -> return Nothing
    Just False -> return $ Just result
findFailure model (Conjunction subs)     = do
  subFailures <- mapM (findFailure model) subs
  return $ listToMaybe $ catMaybes $ subFailures
findFailure model (Condition cond sub)   = do
  cond' <- codegen cond
  val <- evalBool model cond'
  case val of
    Nothing    -> return Nothing
    Just True  -> findFailure model sub
    Just False -> return Nothing
