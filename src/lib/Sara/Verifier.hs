{-# LANGUAGE FlexibleContexts #-}

module Sara.Verifier ( z3Expression ) where

import Sara.Z3AstUtils
import Data.List
import Sara.Operators
import qualified Sara.Syntax as S
import qualified Control.Monad.Trans.Reader as R
import Sara.Types
import Sara.Meta
import Control.Monad.Trans.Class
import Z3.Monad

instance MonadZ3 m => MonadZ3 (R.ReaderT a m) where
  getSolver = lift getSolver
  getContext = lift getContext

z3Sort :: MonadZ3 z3 => Type -> z3 Sort
z3Sort Boolean = mkBoolSort
z3Sort Integer = mkIntSort
z3Sort _       = undefined

z3Symbol :: MonadZ3 z3 => String -> String -> Int -> z3 Symbol
z3Symbol prefix name index = mkStringSymbol $ intercalate "$" [prefix, name, show index]

z3Var :: MonadZ3 z3 => VariableMeta -> Type -> z3 AST
z3Var (VariableMeta name index) typ = do
  sym <- z3Symbol "var" name index
  sort <- z3Sort typ
  mkVar sym sort

z3FuncDecls :: MonadZ3 z3 => FunctionMeta -> [Type] -> Type -> z3 (FuncDecl, FuncDecl, FuncDecl)
z3FuncDecls (FunctionMeta name index) argTypes retType = do
  preSym <- z3Symbol "pre" name index
  postSym <- z3Symbol "post" name index
  funcSym <- z3Symbol "func" name index
  argSorts <- mapM z3Sort argTypes
  retSort <- z3Sort retType
  (,,) <$> mkFuncDecl preSym argSorts retSort <*> mkFuncDecl postSym argSorts retSort <*> mkFuncDecl funcSym argSorts retSort

-- | Create an expression and its precondition and postcondition.
z3Expression :: MonadZ3 z3 => SymbolizerExpression -> z3 CondAST
z3Expression (S.Boolean b _)              = trivial =<< mkBool b
z3Expression (S.Integer n _)              = trivial =<< mkInteger n
z3Expression (S.UnaryOperation op e _)    = do
  e' <- z3Expression e
  z3UnOp op e'
z3Expression (S.BinaryOperation op l r _) = do
  l' <- z3Expression l
  r' <- z3Expression r
  z3BinOp op l' r'
z3Expression v@(S.Variable _ m _)         = trivial =<< z3Var m (expressionTyp v)
z3Expression (S.Conditional c t e _)      = do
  c' <- z3Expression c
  t' <- conditionOn (ast c') =<< z3Expression t
  e' <- conditionOnNot (ast c') =<< z3Expression e
  combine3 mkIte c' t' e'
z3Expression c@(S.Call _ a m _)           = do
  a' <- mapM z3Expression a
  let args = map ast a'
  let aTyps = map expressionTyp a
  let rTyp = expressionTyp c
  (funcPre, funcPost, func) <- z3FuncDecls m aTyps rTyp
  pre <- mkApp funcPre args
  post <- mkApp funcPost args
  addPrecondition pre =<< addPostcondition post =<< combine (mkApp func) a'
z3Expression _                            = undefined

mkZero :: MonadZ3 z3 => z3 AST
mkZero = mkInteger 0

mkNeZero :: MonadZ3 z3 => AST -> z3 AST
mkNeZero b = mkNot =<< mkEq b =<< mkZero
  
z3UnOp :: MonadZ3 z3 => UnaryOperator -> CondAST -> z3 CondAST
z3UnOp op = liftCond $ z3UnOp' op
  where z3UnOp' UnaryPlus e  = return e
        z3UnOp' UnaryMinus e = do
          z <- mkZero
          mkSub [z, e]
        z3UnOp' LogicalNot e = mkNot e
        z3UnOp' _ _          = undefined

-- | Apply a function that takes a list to two arguments.
app2 :: ([a] -> b) -> a -> a -> b
app2 f a b = f [a, b]

z3DivOp :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> CondAST -> CondAST -> z3 CondAST
z3DivOp f l r = do
  neZero <- mkNeZero (ast r)
  result <- combine2 f l r
  addPrecondition neZero result

appConds2 :: MonadZ3 z3 => ([AST] -> z3 AST) -> CondAST -> CondAST -> z3 CondAST
appConds2 = combine2 . app2

z3BinOp :: MonadZ3 z3 => BinaryOperator -> CondAST -> CondAST -> z3 CondAST
z3BinOp Times           = appConds2 mkMul
z3BinOp DividedBy       = z3DivOp mkDiv
z3BinOp Modulo          = z3DivOp mkMod
z3BinOp Plus            = appConds2 mkAdd
z3BinOp Minus           = appConds2 mkSub
z3BinOp LessThan        = combine2 mkLt
z3BinOp AtMost          = combine2 mkLe
z3BinOp GreaterThan     = combine2 mkGt
z3BinOp AtLeast         = combine2 mkGe
z3BinOp EqualTo         = combine2 mkEq
z3BinOp NotEqualTo      = combine2 $ \a b -> mkNot =<< mkEq a b
z3BinOp LogicalAnd      = \a b -> combine2 (app2 mkAnd) a =<< conditionOn (ast a) b
z3BinOp LogicalXor      = combine2 $ mkXor
z3BinOp LogicalOr       = \a b -> combine2 (app2 mkOr) a =<< conditionOnNot (ast a) b
z3BinOp Implies         = \a b -> combine2 mkImplies a =<< conditionOn (ast a) b
z3BinOp ImpliedBy       = \a b -> combine2 (flip mkImplies) b =<< conditionOn (ast b) a
z3BinOp EquivalentTo    = combine2 $ mkEq
z3BinOp NotEquivalentTo = combine2 $ \a b -> mkNot =<< mkEq a b
z3BinOp _               = undefined
