{-# LANGUAGE FlexibleContexts #-}

module Sara.Verifier (z3Expression, Z3FullContext(..), Z3CheapContext(..), FunctionInfo(..), PrecAST(..), z3PrecExpression) where

import Sara.Operators
import qualified Sara.Syntax as S
import Sara.Types
import Sara.Meta
import Data.Maybe
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad
import Z3.Monad

import qualified Data.Map.Strict as M

data FunctionKey =
  FunctionKey S.Name [Type]
  deriving (Eq, Ord, Show)

-- | Context for the second pass which has all the pre- and postconditions.
data Z3FullContext
  = Z3FullContext { fullVars :: M.Map S.Name AST
                  , funcs :: M.Map FunctionKey FunctionInfo }

data FunctionInfo =
  FunctionInfo { precondition :: AST
               , postcondition :: AST }

-- | Context for the first pass which has only the signatures.
-- I.e. we don't have the preconditions yet.
data Z3CheapContext
  = Z3CheapContext { cheapVars :: M.Map S.Name AST
                   , signatures :: M.Map FunctionKey TypeCheckerSignature }

instance MonadZ3 m => MonadZ3 (R.ReaderT a m) where
  getSolver = lift getSolver
  getContext = lift getContext

-- | An AST and his precondition.
data PrecAST = PrecAST { prec :: AST, ast :: AST }

-- | A trivial expression with no precondition.
z3PrecTrivial :: MonadZ3 z3 => z3 AST -> z3 PrecAST
z3PrecTrivial ast = PrecAST <$> mkTrue <*> ast

-- | Create an expression and its precondition.
z3PrecExpression :: (MonadZ3 z3, MonadReader Z3FullContext z3) => TypeCheckerExpression -> z3 PrecAST
z3PrecExpression (S.Boolean b _)              = z3PrecTrivial $ mkBool b
z3PrecExpression (S.Integer n _)              = z3PrecTrivial $ mkInteger n
z3PrecExpression (S.UnaryOperation op e _)    = do
  PrecAST p e' <- z3PrecExpression e
  PrecAST p <$> z3UnOp op e'
z3PrecExpression (S.BinaryOperation op l r _) = do
  l' <- z3PrecExpression l
  r' <- z3PrecExpression r
  z3PrecBinOp op l' r'
z3PrecExpression (S.Variable v _ _)             = z3PrecTrivial $ asks $ fromJust . M.lookup v . fullVars
z3PrecExpression (S.Conditional b t e _)      = do
  PrecAST p b' <- z3PrecExpression b
  PrecAST q t' <- z3PrecExpression t
  PrecAST r e' <- z3PrecExpression e
  q' <- mkImplies b' q
  r' <- join $ mkImplies <$> mkNot b' <*> pure r
  PrecAST <$> mkAnd [p, q', r'] <*> mkIte b' t' e'
z3PrecExpression _                              = undefined

-- | Create an expression without its precondition.
z3Expression :: (MonadZ3 z3, MonadReader Z3CheapContext z3) => TypeCheckerExpression -> z3 AST
z3Expression (S.Boolean b _)              = mkBool b
z3Expression (S.Integer n _)              = mkInteger n
z3Expression (S.UnaryOperation op e _)    = do
  e' <- z3Expression e
  z3UnOp op e'
z3Expression (S.BinaryOperation op l r _) = do
  l' <- z3Expression l
  r' <- z3Expression r
  z3BinOp op l' r'
z3Expression (S.Variable v _ _)             = asks $ fromJust . M.lookup v . cheapVars
z3Expression (S.Conditional b t e _)      = do
  b' <- z3Expression b
  t' <- z3Expression t
  e' <- z3Expression e
  mkIte b' t' e'
z3Expression _                              = undefined

mkZero :: MonadZ3 z3 => z3 AST
mkZero = mkInteger 0

mkNeZero :: MonadZ3 z3 => AST -> z3 AST
mkNeZero b = mkNot =<< mkEq b =<< mkZero
  
z3UnOp :: MonadZ3 z3 => UnaryOperator -> AST -> z3 AST
z3UnOp UnaryPlus e  = return e
z3UnOp UnaryMinus e = do
  z <- mkZero
  mkSub [z, e]
z3UnOp LogicalNot e = mkNot e
z3UnOp _ _          = undefined

-- | Apply a function that takes a list to two arguments.
app2 :: ([a] -> b) -> a -> a -> b
app2 f a b = f [a, b]

-- | For a two argument function that transforms two AST to an AST, just conjunct the preconditions.
forwardPrecs2 :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> PrecAST -> PrecAST -> z3 PrecAST
forwardPrecs2 f (PrecAST p a) (PrecAST q b) = PrecAST <$> mkAnd [p, q] <*> f a b

z3PrecDivOp :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> PrecAST -> PrecAST -> z3 PrecAST
z3PrecDivOp f (PrecAST p a) (PrecAST q b) = do
  neZero <- mkNeZero b
  PrecAST <$> mkAnd [p, neZero, q] <*> f a b

appPrecs2 :: MonadZ3 z3 => ([AST] -> z3 AST) -> PrecAST -> PrecAST -> z3 PrecAST
appPrecs2 = forwardPrecs2 . app2

z3BinOp :: MonadZ3 z3 => BinaryOperator -> AST -> AST -> z3 AST
z3BinOp op a b = ast <$> z3TrivialPrecBinOp
  where z3TrivialPrecBinOp :: MonadZ3 z3 => z3 PrecAST
        z3TrivialPrecBinOp = join $ z3PrecBinOp op <$> z3PrecTrivial (return a) <*> z3PrecTrivial (return b)

z3PrecBinOp :: MonadZ3 z3 => BinaryOperator -> PrecAST -> PrecAST -> z3 PrecAST
z3PrecBinOp Times           = appPrecs2 mkMul
z3PrecBinOp DividedBy       = z3PrecDivOp mkDiv
z3PrecBinOp Modulo          = z3PrecDivOp mkMod
z3PrecBinOp Plus            = appPrecs2 mkAdd
z3PrecBinOp Minus           = appPrecs2 mkSub
z3PrecBinOp LessThan        = forwardPrecs2 mkLt
z3PrecBinOp AtMost          = forwardPrecs2 mkLe
z3PrecBinOp GreaterThan     = forwardPrecs2 mkGt
z3PrecBinOp AtLeast         = forwardPrecs2 mkGe
z3PrecBinOp EqualTo         = forwardPrecs2 mkEq
z3PrecBinOp NotEqualTo      = forwardPrecs2 $ \a b ->
  mkNot =<< mkEq a b
z3PrecBinOp LogicalAnd      = \(PrecAST p a) (PrecAST q b) -> do
  q' <- mkImplies a q
  PrecAST <$> mkAnd [p, q'] <*> mkAnd [a, b]
z3PrecBinOp LogicalXor      = forwardPrecs2 $ mkXor
z3PrecBinOp LogicalOr       =  \(PrecAST p a) (PrecAST q b) -> do
  q' <- join $ mkImplies <$> mkNot a <*> pure q
  PrecAST <$> mkAnd [p, q'] <*> mkOr [a, b]
z3PrecBinOp Implies         = forwardPrecs2 $ mkImplies
z3PrecBinOp ImpliedBy       = forwardPrecs2 $ flip mkImplies
z3PrecBinOp EquivalentTo    = forwardPrecs2 $ mkEq
z3PrecBinOp NotEquivalentTo = forwardPrecs2 $ \a b ->
  mkNot =<< mkEq a b
z3PrecBinOp _               = undefined
