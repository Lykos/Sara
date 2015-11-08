{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Helper functions to be used by the verifier to transform our AST into a Z3 AST.
module Sara.Z3.CondAst ( CondAST
                       , W.FailureTrackableAST
                       , ast
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
                       , W.findFailure ) where

import Z3.Monad
import qualified Sara.Z3.AstWrapper as W
import Sara.Errors (VerifierFailureType)
import Text.Parsec.Pos (SourcePos)

-- | An AST and his proof obligations and assumptions.
data CondAST
  = CondAST { obl :: W.ProofObligation
            , ass :: W.Assumption
            , ast :: AST }
  deriving (Eq, Ord, Show)

-- | A trivial expression with no proof obligation.
trivial :: MonadZ3 z3 => AST -> z3 CondAST
trivial ast = return $ CondAST W.empty W.empty ast

runCondAst :: CondAST -> (W.ProofObligation, W.Assumption, AST)
runCondAst (CondAST obl ass ast) = (obl, ass, ast)

-- | Transforms the proof obligations and assumptions to be conditioned on the given condition.
conditionOn :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
conditionOn cond (CondAST obl ass ast) = return $ CondAST (W.conditionOn cond obl) (W.conditionOn cond ass) ast

-- | Transforms the proof obligations and assumption to be conditioned on NOT the given condition.
conditionOnNot :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
conditionOnNot cond condAst = mkNot cond >>= flip conditionOn condAst

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine2 :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> CondAST -> CondAST -> z3 CondAST
combine2 f (CondAST oblA assA a) (CondAST oblB assB b) =
  CondAST (W.conjunct [oblA, oblB]) (W.conjunct [assA, assB]) <$> f a b

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine3 :: MonadZ3 z3 => (AST -> AST -> AST -> z3 AST) -> CondAST -> CondAST -> CondAST -> z3 CondAST
combine3 f (CondAST oblA assA a) (CondAST oblB assB b) (CondAST oblC assC c) =
  CondAST (W.conjunct [oblA, oblB, oblC]) (W.conjunct [assA, assB, assC]) <$> f a b c

-- | Combine the ast using the given function and conjunct the proof obligations and assumptions.
combine :: MonadZ3 z3 => ([AST] -> z3 AST) -> [CondAST] -> z3 CondAST
combine f conds = CondAST (W.conjunct (map obl conds)) (W.conjunct (map ass conds)) <$> f (map ast conds)

-- | Map the expression, leave the proof obligations and assumptions untouched.
liftCond :: MonadZ3 z3 => (AST -> z3 AST) -> CondAST -> z3 CondAST
liftCond f (CondAST obl ass ast) = CondAST obl ass <$> f ast

-- | Adds the first argument as the proof obligation to the second argument.
addProofObligation :: MonadZ3 z3 => AST -> VerifierFailureType -> SourcePos -> CondAST -> z3 CondAST
addProofObligation newObl failureType pos (CondAST obl ass ast) = return $ CondAST (W.conjunct [newObl', obl]) ass ast
  where newObl' = W.singleton newObl (failureType, pos)

-- | Adds the first argument as the assumption to the second argument.
addAssumption :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
addAssumption newAss (CondAST obl ass ast) = return $ CondAST obl (W.conjunct [newAss', ass]) ast
  where newAss' = W.singleton newAss ()
