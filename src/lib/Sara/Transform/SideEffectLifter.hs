{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Sara.Transform.SideEffectLifter ( liftSideEffects ) where

import Control.Monad.State.Strict ( MonadState, get, modify, evalState )
import Control.Monad.Writer.Strict ( MonadWriter, tell, runWriterT )
import qualified Sara.Parser.PrettyPrinter as P
import qualified Sara.Ast.Syntax as Sy
import qualified Sara.Ast.Operators as O
import qualified Sara.Ast.Meta as M
import qualified Sara.Ast.Types as T

type Expression = M.PureCheckerExpression

liftSideEffects :: Expression -> M.ExpressionMeta -> M.NodeMeta -> Expression
liftSideEffects = sideEffectBlock . flip evalState 0 . runLiftSideEffects

getTmpVar :: MonadState Int m => T.Type -> M.NodeMeta -> m Expression
getTmpVar t n = do
  modify $ (+) 1
  i <- get
  let meta = M.TmpVar t M.SideEffectLifter i
  return $ Sy.Variable ("tmp$SideEffectLifter$" ++ show i) meta (M.ExpressionMeta t True) n

assign :: MonadWriter [Expression] m => Expression -> Expression -> M.ExpressionMeta -> M.NodeMeta -> m ()
assign target exp expMeta nodeMeta = emit $ Sy.BinaryOperation O.Assign target exp expMeta nodeMeta

emit :: MonadWriter [Expression] m => Expression -> m ()
emit = tell . (:[])

type BlockCandidate = (Expression, [Expression])

sideEffectBlock :: BlockCandidate -> M.ExpressionMeta -> M.NodeMeta -> Expression
sideEffectBlock (exp, []) _ _                 = exp
sideEffectBlock (exp, stmts) expMeta nodeMeta = Sy.Block stmts exp expMeta nodeMeta

assignAndReturnTmpVar :: (MonadState Int m, MonadWriter [Expression] m) => Expression -> m Expression
assignAndReturnTmpVar exp = do
  let expMeta' = Sy.expMeta exp
  let nodeMeta' = Sy.nodeMeta exp
  let t = M.expTyp expMeta'
  v <- getTmpVar t nodeMeta'
  assign v exp expMeta' nodeMeta'
  return v

runLiftSideEffects :: MonadState Int m => Expression -> m BlockCandidate
runLiftSideEffects e = runWriterT $ liftSideEffectsInternal e

conditional :: (MonadState Int m, MonadWriter [Expression] m) => Expression -> BlockCandidate -> BlockCandidate -> M.ExpressionMeta -> M.NodeMeta -> m Expression
conditional cond thenBlockCandidate elseBlockCandidate expMeta nodeMeta = do
  let thenExp = sideEffectBlock thenBlockCandidate expMeta nodeMeta
  let elseExp = sideEffectBlock elseBlockCandidate expMeta nodeMeta
  let result = Sy.Conditional cond thenExp elseExp expMeta nodeMeta
  case (snd thenBlockCandidate, snd elseBlockCandidate) of
    ([], []) -> return result
    _        -> assignAndReturnTmpVar result

maybeNegate :: O.MaybeNegation a => a -> Expression -> Expression
maybeNegate a = if O.isPositive a then id else negate
  where negate e = Sy.UnaryOperation O.LogicalNot e (Sy.expMeta e) (Sy.nodeMeta e)

liftSideEffectsInternal :: (MonadState Int m, MonadWriter [Expression] m) => Expression -> m Expression
liftSideEffectsInternal u@Sy.Unit{}             = return u
liftSideEffectsInternal b@Sy.Boolean{}          = return b
liftSideEffectsInternal n@Sy.Integer{}          = return n
liftSideEffectsInternal d@Sy.Double{}           = return d
liftSideEffectsInternal Sy.UnaryOperation{..}   = do
  inner' <- liftSideEffectsInternal inner
  return $ Sy.UnaryOperation unOp inner' expMeta{ M.expPure = True } expNodeMeta
liftSideEffectsInternal Sy.BinaryOperation{..}  = case binOp of
  O.Assign -> do
    left'  <- liftSideEffectsInternal left
    right' <- liftSideEffectsInternal right
    assign left' right' expMeta expNodeMeta
    return left'
  _        -> case O.shortCircuitKind binOp of
    Just O.ShortCircuitKind{..} -> do
      left' <- liftSideEffectsInternal left
      let leftResult = maybeNegate valueWhenPredetermined left'

      let isPredetermined = maybeNegate predeterminedForValue left'

      (right', rightSideEffects) <- runLiftSideEffects right
      let rightResult = maybeNegate valueWhenNotPredetermined right'
      conditional isPredetermined (leftResult, []) (rightResult, rightSideEffects) expMeta expNodeMeta
    Nothing                   -> do
      left'  <- liftSideEffectsInternal left
      right' <- liftSideEffectsInternal right
      let result = Sy.BinaryOperation binOp left' right' expMeta expNodeMeta
      -- Crashing because of division by 0 is a bit like a side effect.
      case binOp of
        O.DividedBy -> assignAndReturnTmpVar result
        O.Modulo    -> assignAndReturnTmpVar result
        _           -> return result
liftSideEffectsInternal v@Sy.Variable{}         = return v
liftSideEffectsInternal Sy.Call{..}             = do
  args' <- mapM liftSideEffectsInternal expArgs
  let expMeta' = expMeta{ M.expPure = M.funcSymPure expCallMeta }
  assignAndReturnTmpVar $ Sy.Call expName args' expCallMeta expMeta' expNodeMeta
liftSideEffectsInternal Sy.Conditional{..}      = do
  cond' <- liftSideEffectsInternal cond
  thenPart <- runLiftSideEffects thenExp
  elsePart <- runLiftSideEffects elseExp
  conditional cond' thenPart elsePart expMeta expNodeMeta
liftSideEffectsInternal Sy.Block{..}            = do
  mapM_ liftSideEffectsInternal stmts
  liftSideEffectsInternal inner
liftSideEffectsInternal Sy.While{..}            = do
  v <- getTmpVar T.Boolean expNodeMeta
  cond' <- liftSideEffectsInternal cond
  assign v cond' expMeta expNodeMeta
  let unit = Sy.Unit expMeta expNodeMeta
  innerPart <- runWriterT $ do
    -- We throw away the side effect free expression of inner.
    liftSideEffectsInternal inner
    cond'' <- liftSideEffectsInternal cond
    assign v cond'' expMeta expNodeMeta
    return unit
  let innerResult = sideEffectBlock innerPart expMeta expNodeMeta
  emit $ Sy.While invariants v innerResult expMeta expNodeMeta
  return unit
liftSideEffectsInternal Sy.Assertion{..}        = do
  inner' <- liftSideEffectsInternal inner
  emit $ Sy.Assertion assertionKind inner' expMeta expNodeMeta
  return $ Sy.Unit expMeta expNodeMeta
liftSideEffectsInternal e@Sy.VarDef{}           = error $ "Lifting side effects for VarDef " ++ P.prettyRender e ++ " is not supported."
