-- | Simple common Z3 utilities that are used by multiple modules.

module Sara.Z3.Utils ( z3Sort
                     , z3VarName
                     , z3Var
                     , z3FuncDecls
                     , conjunctAsts
                     , mkZero
                     , mkNeZero ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Z3.Monad
import Sara.Meta
import Sara.Types
import Data.List

-- | Conjuncts the given asts, but pays attention to empty or singleton lists because Z3 doesn't like empty or singleton conjunctions.
conjunctAsts :: MonadZ3 z3 => [AST] -> z3 AST
conjunctAsts []    = mkTrue
conjunctAsts [ast] = return ast
conjunctAsts as    = mkAnd as

-- | Translates a type to a Z3 sort.
z3Sort :: MonadZ3 z3 => Type -> z3 Sort
z3Sort Boolean = mkBoolSort
z3Sort Integer = mkIntSort
z3Sort t       = error $ "Unsupported type for verifier: " ++ show t

-- | Creates a Z3 variable name from the given components.
z3VarName :: [String] -> String
z3VarName = intercalate "$"

z3Symbol :: MonadZ3 z3 => String -> String -> Int -> z3 Symbol
z3Symbol prefix name index = mkStringSymbol $ z3VarName [prefix, name, show index]

z3Var :: MonadZ3 z3 => VariableMeta -> z3 AST
z3Var (VariableMeta typ name index) = do
  sym <- z3Symbol "var" name index
  sort <- z3Sort typ
  mkVar sym sort

-- | Creates three function declarations for a function or method in the source file:
-- * One function declarations for the function itself.
-- * One function that evaluates its preconditions as a boolean.
-- * One function that evaluates its postconditions as a boolean.
z3FuncDecls :: MonadZ3 z3 => FunctionMeta -> z3 (FuncDecl, FuncDecl, FuncDecl)
z3FuncDecls (FunctionMeta isPure argTypes retType name index) = do
  preSym <- z3Symbol "pre" name index
  postSym <- z3Symbol "post" name index
  funcSym <- z3Symbol "func" name index
  argSorts <- mapM z3Sort argTypes
  retSort <- z3Sort retType
  boolSort <- mkBoolSort
  funcDecl <- case isPure of
    True -> mkFuncDecl funcSym argSorts retSort
    False -> return $ error "Non-pure functions cannot be called in Z3."
  (,,) <$> mkFuncDecl preSym argSorts boolSort <*> mkFuncDecl postSym argSorts boolSort <*> pure funcDecl

mkZero :: MonadZ3 z3 => z3 AST
mkZero = mkInteger 0

mkNeZero :: MonadZ3 z3 => AST -> z3 AST
mkNeZero b = mkNot =<< mkEq b =<< mkZero

instance MonadZ3 m => MonadZ3 (ExceptT a m) where
  getSolver = lift getSolver
  getContext = lift getContext

instance MonadZ3 m => MonadZ3 (StateT s m) where
  getSolver = lift getSolver
  getContext = lift getContext

instance MonadZ3 m => MonadZ3 (ReaderT s m) where
  getSolver = lift getSolver
  getContext = lift getContext
  
instance (Monoid w, MonadZ3 m) => MonadZ3 (WriterT w m) where
  getSolver = lift getSolver
  getContext = lift getContext
