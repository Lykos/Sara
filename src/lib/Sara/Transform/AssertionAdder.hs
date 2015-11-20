-- | Module to make all the assertions and assumptions (i.e. pre- and postconditions etc.) explicit.

module Sara.Transform.AssertionAdder ( addAssertions ) where

import Control.Monad.State.Strict ( MonadState, get, modify, evalState )
import qualified Sara.Ast.AstUtils as A
import qualified Sara.Ast.Syntax as S
import qualified Sara.Ast.Meta as M
import qualified Data.Map as Map

type AssertionMap = Map.Map M.FunctionMeta ([M.PureCheckerExpression], [M.PureCheckerExpression])

addAssertions :: AssertionMap -> M.PureCheckerProgram -> M.PureCheckerProgram
addAssertions assertionMap = flip runReaderT assertionMap . flip runState 0 . A.mapMExpressions addAssertion

getTmpVar :: MonadState Int m => T.Type -> M.NodeMeta -> m Expression
getTmpVar t n = do
  modify $ (+) 1
  i <- get
  let meta = M.TmpVar t M.AssertionAdder i
  return $ Sy.Variable ("tmp$SideEffectLifter$" ++ show i) meta (M.ExpressionMeta t True) n

addAssertion (MonadReader AssertionMap m, MonadState Int m) => M.PureCheckerExpression -> m PureCheckerExpression
addAssertion c@S.Call{..} = do
  assertions <- asks $ fromMaybe (error "Sara.Transform.AssertionAdder.addAssertion") . M.lookup expCallMeta
  case assertions of
    ([], [])       -> return c
    (precs, [])    -> return $ Block (assume precs) c expMeta expNodeMeta
    (precs, posts) -> do
      let t = M.expressionTyp expMeta
      v <- getTmpVar t expNodeMeta
      Block (assert precs ++ [assign v c] ++ assume posts) v expMeta expNodeMeta
addAssertions o@S.BinaryOperation{ binOp = binOp } =
  case binOp of
    
  

