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
                       , addPrecondition
                       , addPostcondition
                       , findFailure ) where

import Data.Maybe
import Z3.Monad
import Sara.Errors (VerifierFailureType)
import Text.Parsec.Pos (SourcePos)

-- | A trivial expression with no precondition.
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

-- | Ast Wrapper for preconditions.
-- We use this to store additional information with the AST and to ensure better type safety.
newtype PreAST
  = PreAST { unPre :: FailureTrackableAST }
  deriving (Eq, Ord, Show)

-- | Ast Wrapper for postconditions.
-- We use this to for better type safety.
newtype PostAST
  = PostAST { unPost :: TrackableAST () }
  deriving (Eq, Ord, Show)

instance ASTWrapper PreAST (VerifierFailureType, SourcePos) where
  wrap = PreAST
  unwrap = unPre

instance ASTWrapper PostAST () where
  wrap = PostAST
  unwrap = unPost

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

-- | An AST and his pre- and postcondition.
data CondAST = CondAST { pre :: PreAST, post :: PostAST, ast :: AST }

runCondAst :: MonadZ3 z3 => CondAST -> z3 (FailureTrackableAST, AST, AST)
runCondAst (CondAST pre post ast) = (,,) (unwrap pre) <$> runAst post <*> pure ast

-- | Transforms the prec- and postcondition to be conditioned on the given condition.
conditionOn :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
conditionOn cond (CondAST pre post ast) = return $ CondAST (condition cond pre) (condition cond post) ast

-- | Transforms the prec- and postcondition to be conditioned on NOT the given condition.
conditionOnNot :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
conditionOnNot cond condAst = mkNot cond >>= flip conditionOn condAst

-- | Combine the ast using the given function and conjunct the pre- and postconditions.
combine2 :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> CondAST -> CondAST -> z3 CondAST
combine2 f (CondAST preA postA a) (CondAST preB postB b) =
  CondAST (conjunct [preA, preB]) (conjunct [postA, postB]) <$> f a b

-- | Combine the ast using the given function and conjunct the pre- and postconditions.
combine3 :: MonadZ3 z3 => (AST -> AST -> AST -> z3 AST) -> CondAST -> CondAST -> CondAST -> z3 CondAST
combine3 f (CondAST preA postA a) (CondAST preB postB b) (CondAST preC postC c) =
  CondAST (conjunct [preA, preB, preC]) (conjunct [postA, postB, postC]) <$> f a b c

-- | Combine the ast using the given function and conjunct the pre- and postconditions.
combine :: MonadZ3 z3 => ([AST] -> z3 AST) -> [CondAST] -> z3 CondAST
combine f conds = CondAST (conjunct (map pre conds)) (conjunct (map post conds)) <$> f (map ast conds)

-- | Map the expression, leave the pre- and postconditions untouched.
liftCond :: MonadZ3 z3 => (AST -> z3 AST) -> CondAST -> z3 CondAST
liftCond f (CondAST pre post ast) = CondAST pre post <$> f ast

-- | Adds the first argument as the precondition to the second argument.
addPrecondition :: MonadZ3 z3 => AST -> VerifierFailureType -> SourcePos -> CondAST -> z3 CondAST
addPrecondition newPre failureType pos (CondAST pre post ast) = return $ CondAST (conjunct [newPre', pre]) post ast
  where newPre' = singleton newPre (failureType, pos)

-- | Adds the first argument as the postcondition to the second argument.
addPostcondition :: MonadZ3 z3 => AST -> CondAST -> z3 CondAST
addPostcondition newPost (CondAST pre post ast) = return $ CondAST pre (conjunct [newPost', post]) ast
  where newPost' = singleton newPost ()
