{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Sara.Transform.SideEffectLifter ( liftSideEffects ) where

import Control.Monad.State.Strict as St
import Sara.Ast.Syntax as Sy
import Sara.Ast.Operators as O

purified :: ExpressionMeta -> ExpressionMeta
purified e = e{ expPure = True }

liftSideEffectsInternal :: St.MonadState SideEffectState m => PureCheckerExpression -> m PureCheckerExpression
liftSideEffectsInternal e | expPure $ expMeta e = return e
liftSideEffectsInternal Sy.UnaryOperation{..}   = do
  inner' <- liftSideEffectsInternal inner
  return $ Sy.UnaryOperation unOp inner (purified expMeta) expNodeMeta
liftSideEffectsInternal Sy.BinaryOperation{..}  = case O.shortCircuitKind binOp of
  Just kind -> do
  Nothing   -> do
liftSideEffectsInternal Sy.Call{..}             = do
  args' <- mapM liftSideEffectsInternal expArgs
  if isPure then
    return $ Call expName args' expCallMeta (purified expMeta) expNodeMeta
  else do
    v <- 
