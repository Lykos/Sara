module AstUtils where

import Operators
import Syntax
import Control.Monad
import Control.Monad.Identity

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

transformToMap :: ((a -> Identity a) -> (b -> Identity b)) -> (a -> a) -> b -> b
transformToMap transform f = runIdentity . transform (Identity . f)

mapExpressionAst :: (ExpressionAst -> ExpressionAst) -> ExpressionAst -> ExpressionAst
mapExpressionAst = transformToMap transformExpressionAst

transformExpressionAsts :: Monad m => (ExpressionAst -> m ExpressionAst) -> Program -> m Program
transformExpressionAsts = transformDeclarationAsts . liftDeclaration

mapExpressionAsts :: (ExpressionAst -> ExpressionAst) -> Program -> Program
mapExpressionAsts = transformToMap transformExpressionAsts

liftDeclaration :: Monad m => (ExpressionAst -> m ExpressionAst) -> DeclarationAst -> m DeclarationAst
liftDeclaration f (DeclarationAst (Function sig exp) pos) = do
  exp' <- transformExpressionAst f exp
  let func = Function sig exp'
  return $ DeclarationAst func pos
liftDeclaration _ d                                        = return d

transformDeclarationAsts :: Monad m => (DeclarationAst -> m DeclarationAst) -> Program -> m Program
transformDeclarationAsts f = liftM Program . ((sequence . map f) . program)

mapDeclarationAsts :: (DeclarationAst -> DeclarationAst) -> Program -> Program
mapDeclarationAsts = transformToMap transformDeclarationAsts
