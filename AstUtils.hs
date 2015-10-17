module AstUtils (
  FunctionOrMethodConstructor
  , transformExpressionAst
  , mapExpressionAst
  , transformExpressionAsts
  , mapExpressionAsts
  , mapDeclarationAsts
  , foldMapExpressionAst
  , foldMapExpressionAsts
  , foldMapExpressions
  , transformDeclarationAsts
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

type FunctionOrMethodConstructor = Signature -> ExpressionAst -> Declaration

transformExpressionAst :: Monad m => (ExpressionAst -> m ExpressionAst) -> ExpressionAst -> m ExpressionAst
transformExpressionAst f (ExpressionAst exp typ pos) = do
  exp' <- transformedExp
  f $ ExpressionAst exp' typ pos
    where transformSubExp = transformExpressionAst f
          transformedExp = case exp of
            BinaryOperation op left right  -> liftM2 (BinaryOperation op) (transformSubExp left) (transformSubExp right)
            UnaryOperation op exp          -> liftM (UnaryOperation op) (transformSubExp exp)
            Conditional cond ifExp elseExp -> liftM3 Conditional (transformSubExp cond) (transformSubExp ifExp) (transformSubExp elseExp)
            Call name args                 -> liftM (Call name) (sequence $ map transformSubExp args)
            e                              -> return e

foldMapExpressionAst :: Monoid m => (ExpressionAst -> m) -> ExpressionAst -> m
foldMapExpressionAst = transformToFoldMap transformExpressionAst

transformToFoldMap :: Monoid m => ((a -> State m a) -> (b -> State m b)) -> (a -> m) -> b -> m
transformToFoldMap transform f e = execState (transform accumulate e) mempty
  where accumulate e = (modify . mappend . f) e >> return e

transformToMap :: ((a -> Identity a) -> (b -> Identity b)) -> (a -> a) -> b -> b
transformToMap transform f = runIdentity . transform (Identity . f)

mapExpressionAst :: (ExpressionAst -> ExpressionAst) -> ExpressionAst -> ExpressionAst
mapExpressionAst = transformToMap transformExpressionAst

transformExpressionAsts :: Monad m => (ExpressionAst -> m ExpressionAst) -> Program -> m Program
transformExpressionAsts = transformDeclarationAsts . liftDeclaration

transformExpressions :: Monad m => (Expression -> m Expression) -> Program -> m Program
transformExpressions f = transformExpressionAsts transformExpressionAst
  where transformExpressionAst (ExpressionAst exp typ pos) = do
          exp' <- f exp
          return $ ExpressionAst exp' typ pos

foldMapExpressions :: Monoid m => (Expression -> m) -> Program -> m
foldMapExpressions = transformToFoldMap transformExpressions

foldMapExpressionAsts :: Monoid m => (ExpressionAst -> m) -> Program -> m
foldMapExpressionAsts = transformToFoldMap transformExpressionAsts

mapExpressionAsts :: (ExpressionAst -> ExpressionAst) -> Program -> Program
mapExpressionAsts = transformToMap transformExpressionAsts

liftDeclaration :: Monad m => (ExpressionAst -> m ExpressionAst) -> DeclarationAst -> m DeclarationAst
liftDeclaration f (DeclarationAst (Function sig exp) pos) = liftFunctionOrMethod Function f sig exp pos
liftDeclaration f (DeclarationAst (Method sig exp) pos)   = liftFunctionOrMethod Method f sig exp pos
liftDeclaration _ d                                       = return d

liftFunctionOrMethod :: Monad m => FunctionOrMethodConstructor -> (ExpressionAst -> m ExpressionAst) -> Signature -> ExpressionAst -> SourcePos -> m DeclarationAst
liftFunctionOrMethod constructor f sig exp pos = do
  exp' <- transformExpressionAst f exp
  let func = constructor sig exp'
  return $ DeclarationAst func pos

transformDeclarationAsts :: Monad m => (DeclarationAst -> m DeclarationAst) -> Program -> m Program
transformDeclarationAsts f = liftM Program . ((sequence . map f) . program)

mapDeclarationAsts :: (DeclarationAst -> DeclarationAst) -> Program -> Program
mapDeclarationAsts = transformToMap transformDeclarationAsts

transformSignatures :: Monad m => (Signature -> m Signature) -> Program -> m Program
transformSignatures f = transformDeclarationAsts transformDeclarationAst
  where transformDeclarationAst (DeclarationAst decl pos) = do
          decl' <- transformDeclaration decl
          return $ DeclarationAst decl' pos
        transformDeclaration (Function sig body) = do
          sig' <- f sig
          return $ Function sig' body
        transformDeclaration (Method sig body) = do
          sig' <- f sig
          return $ Method sig' body
        transformDeclaration (Extern sig) = do
          sig' <- f sig
          return $ Extern sig'

mapSignatures :: (Signature -> Signature) -> Program -> Program
mapSignatures = transformToMap transformSignatures

foldMapSignatures :: Monoid m => (Signature -> m) -> Program -> m
foldMapSignatures = transformToFoldMap transformSignatures
