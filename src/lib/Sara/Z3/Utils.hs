-- | Simple common Z3 utilities that are used by multiple modules.

module Sara.Z3.Utils ( z3Name
                     , z3VarName
                     , z3FuncName ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Z3.Monad
import Sara.Meta
import Sara.Z3.Ast
import Data.List

-- | Creates a Z3 name from the given components.
z3Name :: [String] -> String
z3Name = intercalate "$"

z3SymbolName :: String -> String -> Int -> String
z3SymbolName prefix name index = z3Name [prefix, name, show index]

z3VarName :: VariableMeta -> String
z3VarName (VariableMeta _ name index) = z3SymbolName "var" name index

appPrefix :: AppKind -> String
appPrefix PreApp  = "pre"
appPrefix PostApp = "post"
appPrefix FuncApp = "func"
appPrefix FakeApp = "fakeFunc"

z3FuncName :: AppMeta -> String
z3FuncName (AppMeta kind (FunctionMeta _ _ _ name index)) = z3SymbolName (appPrefix kind) name index

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
