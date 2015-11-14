{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Sara.Z3.CondAst ( CondAst
                       , W.FailureTrackableAst
                       , ast
                       , runCondAst
                       , trivial
                       , conditionOn
                       , conditionOnNot
                       , combine2
                       , combine3
                       , combineN
                       , liftCond
                       , addProofObligation
                       , addAssumption
                       , W.findFailure ) where

import qualified Sara.Z3.Ast as A
import qualified Sara.Z3.AstWrapper as W
import Sara.Errors (VerifierFailureType)
import Text.Parsec.Pos (SourcePos)

-- | An A.Ast and his proof obligations and assumptions.
data CondAst
  = CondAst { obl :: W.ProofObligation
            , ass :: W.Assumption
            , ast :: A.Ast }
  deriving (Eq, Ord, Show)

-- | A trivial expression with no proof obligation.
trivial :: A.Ast -> CondAst
trivial ast = CondAst W.empty W.empty ast

runCondAst :: CondAst -> (W.ProofObligation, W.Assumption, A.Ast)
runCondAst (CondAst obl ass ast) = (obl, ass, ast)

-- | Transforms the proof obligations and assumptions to be conditioned on the given condition.
conditionOn :: A.Ast -> CondAst -> CondAst
conditionOn cond (CondAst obl ass ast) = CondAst (W.conditionOn cond obl) (W.conditionOn cond ass) ast

-- | Transforms the proof obligations and assumption to be conditioned on NOT the given condition.
conditionOnNot :: A.Ast -> CondAst -> CondAst
conditionOnNot cond condAst = conditionOn (A.UnOp A.Not cond) condAst

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine2 :: (A.Ast -> A.Ast -> A.Ast) -> CondAst -> CondAst -> CondAst
combine2 f (CondAst oblA assA a) (CondAst oblB assB b) =
  CondAst (W.conjunct [oblA, oblB]) (W.conjunct [assA, assB]) $ f a b

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine3 :: (A.Ast -> A.Ast -> A.Ast -> A.Ast) -> CondAst -> CondAst -> CondAst -> CondAst
combine3 f (CondAst oblA assA a) (CondAst oblB assB b) (CondAst oblC assC c) =
  CondAst (W.conjunct [oblA, oblB, oblC]) (W.conjunct [assA, assB, assC]) $ f a b c

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combineN :: ([A.Ast] -> A.Ast) -> [CondAst] -> CondAst
combineN f conds = CondAst (W.conjunct (map obl conds)) (W.conjunct (map ass conds)) $ f (map ast conds)

-- | Map the expression, leave the proof obligations and assumptions untouched.
liftCond :: (A.Ast -> A.Ast) -> CondAst -> CondAst
liftCond f (CondAst obl ass ast) = CondAst obl ass $ f ast

-- | Adds the first argument as the proof obligation to the second argument.
addProofObligation :: A.Ast -> VerifierFailureType -> SourcePos -> CondAst -> CondAst
addProofObligation newObl failureType pos (CondAst obl ass ast) = CondAst (W.conjunct [newObl', obl]) ass ast
  where newObl' = W.singleton newObl (failureType, pos)

-- | Adds the first argument as the assumption to the second argument.
addAssumption :: A.Ast -> CondAst -> CondAst
addAssumption newAss (CondAst obl ass ast) = CondAst obl (W.conjunct [newAss', ass]) ast
  where newAss' = W.singleton newAss ()
