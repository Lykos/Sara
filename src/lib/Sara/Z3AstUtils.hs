{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Helper functions to be used by the verifier to transform our AST into a Z3 AST.
module Sara.Z3AstUtils ( CondAST
                       , FailureTrackableAST
                       , conjunctAsts
                       , empty
                       , singleton
                       , conjunct
                       , condition
                       , ast
                       , runAst
                       , runCondAst
                       , trivial
                       , conditionOn
                       , conditionOnNot
                       , combine2
                       , combine3
                       , combine
                       , liftCond
                       , addProofObligation
                       , addAssumption
                       , findFailure ) where

import Data.Maybe
import Z3.Monad
import Sara.Errors (VerifierFailureType)
import Text.Parsec.Pos (SourcePos)

-- | A trivial expression with no proof obligation.
trivial :: MonadZ3 z3 => AST -> z3 CondAST
trivial ast = return $ CondAST empty empty ast

-- | AST structure container for which a failure can be tracked to the failing part.
data TrackableAST a
  = Empty
  | Singleton AST a
  | Conjunction [TrackableAST a]
  | Condition AST (TrackableAST a)
  deriving (Eq, Ord, Show)

-- | AST structure container for which a failure can be tracked.
type FailureTrackableAST = TrackableAST (VerifierFailureType, SourcePos)

class ASTWrapper a b | a -> b where
  unwrap :: a -> TrackableAST b
  wrap :: TrackableAST b -> a

-- | Ast Wrapper for proof obligations.
-- We use this to store additional information with the AST and to ensure better type safety.
newtype ProofObligation
  = ProofObligation { unObl :: FailureTrackableAST }
  deriving (Eq, Ord, Show)

-- | Ast Wrapper for assumptions.
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
  where extract Empty            = []
        extract (Conjunction as) = as
        extract a                = [a]

condition :: ASTWrapper a b => AST -> a -> a
condition ast = wrap . condition' . unwrap
  where condition' Empty = Empty
        condition' a     = Condition ast a

runAst :: (ASTWrapper a b, MonadZ3 z3) => a -> z3 AST
runAst a = runAst' (unwrap a)
  where runAst' Empty             = mkTrue
        runAst' (Singleton ast _) = return ast
        runAst' (Conjunction as)  = conjunctAsts =<< mapM runAst' as
        runAst' (Condition ast a) = mkImplies ast =<< runAst' a

-- | Conjuncts the given asts, but pays attention to empty or singleton lists because Z3 doesn't like empty or singleton conjunctions.
conjunctAsts ::MonadZ3 z3 => [AST] -> z3 AST
conjunctAsts []    = mkTrue
conjunctAsts [ast] = return ast
conjunctAsts as    = mkAnd as

findFailure :: MonadZ3 z3 => Model -> TrackableAST a -> z3 (Maybe a)
findFailure _ Empty                      = return Nothing
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

-- | An AST and his proof obligations and assumptions.
data CondAST
  = CondAST { obl :: ProofObligation
            , ass :: Assumption
            , ast :: AST }
  deriving (Eq, Ord, Show)

runCondAst :: MonadZ3 z3 => CondAST -> z3 (FailureTrackableAST, AST, AST)
runCondAst (CondAST obl ass ast) = (,,) (unwrap obl) <$> runAst ass <*> pure ast

-- | Transforms the proof obligations and assumptions to be conditioned on the given condition.
conditionOn :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
conditionOn cond (CondAST obl ass ast) = return $ CondAST (condition cond obl) (condition cond ass) ast

-- | Transforms the proof obligations and assumption to be conditioned on NOT the given condition.
conditionOnNot :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
conditionOnNot cond condAst = mkNot cond >>= flip conditionOn condAst

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine2 :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> CondAST -> CondAST -> z3 CondAST
combine2 f (CondAST oblA assA a) (CondAST oblB assB b) =
  CondAST (conjunct [oblA, oblB]) (conjunct [assA, assB]) <$> f a b

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine3 :: MonadZ3 z3 => (AST -> AST -> AST -> z3 AST) -> CondAST -> CondAST -> CondAST -> z3 CondAST
combine3 f (CondAST oblA assA a) (CondAST oblB assB b) (CondAST oblC assC c) =
  CondAST (conjunct [oblA, oblB, oblC]) (conjunct [assA, assB, assC]) <$> f a b c

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine :: MonadZ3 z3 => ([AST] -> z3 AST) -> [CondAST] -> z3 CondAST
combine f conds = CondAST (conjunct (map obl conds)) (conjunct (map ass conds)) <$> f (map ast conds)

-- | Map the expression, leave the proof obligations and assumptions untouched.
liftCond :: MonadZ3 z3 => (AST -> z3 AST) -> CondAST -> z3 CondAST
liftCond f (CondAST obl ass ast) = CondAST obl ass <$> f ast

-- | Adds the first argument as the proof obligation to the second argument.
addProofObligation :: MonadZ3 z3 => AST -> VerifierFailureType -> SourcePos -> CondAST -> z3 CondAST
addProofObligation newObl failureType pos (CondAST obl ass ast) = return $ CondAST (conjunct [newObl', obl]) ass ast
  where newObl' = singleton newObl (failureType, pos)

-- | Adds the first argument as the assumption to the second argument.
addAssumption :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
addAssumption newAss (CondAST obl ass ast) = return $ CondAST obl (conjunct [newAss', ass]) ast
  where newAss' = singleton newAss ()
