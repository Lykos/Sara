{-# LANGUAGE FlexibleContexts #-}

module Sara.Z3Expression ( z3Expression ) where

import Sara.PrettyPrinter
import Sara.Errors as E
import Sara.Z3AstUtils
import Sara.Z3Utils
import Sara.Operators
import qualified Sara.Syntax as S
import Sara.Meta
import Z3.Monad
import Text.Parsec.Pos (SourcePos)

-- | Create an expression and its precondition and postcondition.
z3Expression :: MonadZ3 z3 => SymbolizerExpression -> z3 CondAST
z3Expression (S.Boolean b _)                = trivial =<< mkBool b
z3Expression (S.Integer n _)                = trivial =<< mkInteger n
z3Expression (S.UnaryOperation op e _)      = do
  e' <- z3Expression e
  z3UnOp op e'
z3Expression b@(S.BinaryOperation op l r _) = do
  l' <- z3Expression l
  r' <- z3Expression r
  let pos = expressionPos b
  z3BinOp pos op l' r'
z3Expression v@(S.Variable _ m _)           = trivial =<< z3Var m (expressionTyp v)
z3Expression (S.Conditional c t e _)        = do
  c' <- z3Expression c
  t' <- conditionOn (ast c') =<< z3Expression t
  e' <- conditionOnNot (ast c') =<< z3Expression e
  combine3 mkIte c' t' e'
z3Expression c@(S.Call name a m _)          = do
  a' <- mapM z3Expression a
  let args = map ast a'
  let aTyps = map expressionTyp a
  let rTyp = expressionTyp c
  (funcPre, funcPost, func) <- z3FuncDecls m aTyps rTyp
  pre <- mkApp funcPre args
  post <- mkApp funcPost args
  let pos = expressionPos c
  let failureType = PreconditionViolation (E.Function name)
  addProofObligation pre failureType pos =<< addAssumption post =<< combine (mkApp func) a'
z3Expression exp                            = error $ "Unsupported expression for verifier: " ++ prettyRender exp

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
        z3UnOp' op _          = error $ "Unsupported unary operator for verifier: " ++ show op

-- | Apply a function that takes a list to two arguments.
app2 :: ([a] -> b) -> a -> a -> b
app2 f a b = f [a, b]

z3DivOp :: MonadZ3 z3 => SourcePos -> (AST -> AST -> z3 AST) -> CondAST -> CondAST -> z3 CondAST
z3DivOp pos f l r = do
  neZero <- mkNeZero (ast r)
  result <- combine2 f l r
  addProofObligation neZero DivisionByZero pos result

appConds2 :: MonadZ3 z3 => ([AST] -> z3 AST) -> CondAST -> CondAST -> z3 CondAST
appConds2 = combine2 . app2

z3BinOp :: MonadZ3 z3 => SourcePos -> BinaryOperator -> CondAST -> CondAST -> z3 CondAST
z3BinOp _ Times           = appConds2 mkMul
z3BinOp p DividedBy       = z3DivOp p mkDiv
z3BinOp p Modulo          = z3DivOp p mkMod
z3BinOp _ Plus            = appConds2 mkAdd
z3BinOp _ Minus           = appConds2 mkSub
z3BinOp _ LessThan        = combine2 mkLt
z3BinOp _ AtMost          = combine2 mkLe
z3BinOp _ GreaterThan     = combine2 mkGt
z3BinOp _ AtLeast         = combine2 mkGe
z3BinOp _ EqualTo         = combine2 mkEq
z3BinOp _ NotEqualTo      = combine2 $ \a b -> mkNot =<< mkEq a b
z3BinOp _ LogicalAnd      = \a b -> combine2 (app2 mkAnd) a =<< conditionOn (ast a) b
z3BinOp _ LogicalXor      = combine2 $ mkXor
z3BinOp _ LogicalOr       = \a b -> combine2 (app2 mkOr) a =<< conditionOnNot (ast a) b
z3BinOp _ Implies         = \a b -> combine2 mkImplies a =<< conditionOn (ast a) b
z3BinOp _ ImpliedBy       = \a b -> combine2 (flip mkImplies) b =<< conditionOn (ast b) a
z3BinOp _ EquivalentTo    = combine2 $ mkEq
z3BinOp _ NotEquivalentTo = combine2 $ \a b -> mkNot =<< mkEq a b
z3BinOp _ op              = error $ "Unsupported binary operator for verifier: " ++ show op
