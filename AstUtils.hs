module AstUtils (
  transformExpression
  , mapExpression
  , mapExpressions
  , mapDeclarations
  , foldMapExpression
  , foldMapExpressions
  , transformDeclarations
  , transformExpressions
  , transformSignatures
  , mapSignatures
  , foldMapSignatures) where


import Operators
import Syntax
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid
import Text.Parsec.Pos

transformExpression :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
transformExpression f exp = do
  exp' <- transformedExp
  f $ exp' (typ exp) (position exp)
    where transformSubExp = transformExpression f
          transformedExp = case exp of
            BinaryOperation op left right _ _  -> liftM2 (BinaryOperation op) (transformSubExp left) (transformSubExp right)
            UnaryOperation op exp _ _          -> liftM (UnaryOperation op) (transformSubExp exp)
            Conditional cond ifExp elseExp _ _ -> liftM3 Conditional (transformSubExp cond) (transformSubExp ifExp) (transformSubExp elseExp)
            Call name args _ _                 -> liftM (Call name) (mapM transformSubExp args)
            Block stmts exp _ _                -> liftM2 Block (mapM transformSubExp stmts) (transformSubExp exp)
            While cond body _ _                -> liftM2 While (transformSubExp cond) (transformSubExp body)
            e                                  -> return $ const $ const e

foldMapExpression :: Monoid m => (Expression -> m) -> Expression -> m
foldMapExpression = transformToFoldMap transformExpression

transformToFoldMap :: Monoid m => ((a -> State m a) -> b -> State m b) -> (a -> m) -> b -> m
transformToFoldMap transform f e = execState (transform accumulate e) mempty
  where accumulate e = (modify . mappend . f) e >> return e

transformToMap :: ((a -> Identity a) -> b -> Identity b) -> (a -> a) -> b -> b
transformToMap transform f = runIdentity . transform (Identity . f)

mapExpression :: (Expression -> Expression) -> Expression -> Expression
mapExpression = transformToMap transformExpression

transformExpressions :: Monad m => (Expression -> m Expression) -> Program -> m Program
transformExpressions = transformDeclarations . liftDeclaration

foldMapExpressions :: Monoid m => (Expression -> m) -> Program -> m
foldMapExpressions = transformToFoldMap transformExpressions

mapExpressions :: (Expression -> Expression) -> Program -> Program
mapExpressions = transformToMap transformExpressions

liftDeclaration :: Monad m => (Expression -> m Expression) -> Declaration -> m Declaration
liftDeclaration f (Function sig exp pos) = liftFunction f sig exp pos
liftDeclaration _ d                      = return d

liftFunction :: Monad m => (Expression -> m Expression) -> Signature -> Expression -> SourcePos -> m Declaration
liftFunction f sig exp pos = do
  exp' <- transformExpression f exp
  return $ Function sig exp' pos

transformDeclarations :: Monad m => (Declaration -> m Declaration) -> Program -> m Program
transformDeclarations f p@(Program decls _) = do
  decls' <- mapM f decls
  return p{ program = decls' }

mapDeclarations :: (Declaration -> Declaration) -> Program -> Program
mapDeclarations = transformToMap transformDeclarations

transformSignatures :: Monad m => (Signature -> m Signature) -> Program -> m Program
transformSignatures f = transformDeclarations transformDeclaration
  where transformDeclaration (Function sig body pos) = do
          sig' <- f sig
          return $ Function sig' body pos
        transformDeclaration (Extern sig pos) = do
          sig' <- f sig
          return $ Extern sig' pos

mapSignatures :: (Signature -> Signature) -> Program -> Program
mapSignatures = transformToMap transformSignatures

foldMapSignatures :: Monoid m => (Signature -> m) -> Program -> m
foldMapSignatures = transformToFoldMap transformSignatures
