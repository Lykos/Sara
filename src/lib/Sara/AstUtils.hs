module Sara.AstUtils ( mapNodeMetas
                     , mapExpressionMetas
                     , transformExpression
                     , transformProgram
                     , checkExpression
                     , foldMapSignatures
                     , foldMapExpression
                     , foldMapExpressions ) where

import Data.Bifunctor
import Sara.Syntax
import Control.Monad.Writer
import Control.Monad.Identity

-- | Accumulates a monoid over all signatures
foldMapSignatures :: Monoid m => (Signature a b c d -> m) -> Program a b c d -> m
foldMapSignatures f = execWriter . transformProgram id id return return (accumulate f) return

-- | Checks a property about an expression.
checkExpression :: Monad m => (Expression a b c d -> m ()) -> Expression a b c d -> m ()
checkExpression expCheck exp = transformExpression id id check exp >> return ()
  where check exp = expCheck exp >> return exp

-- | Accumulates a monoid over an expression.
foldMapExpression :: Monoid m => (Expression a b c d -> m) -> Expression a b c d -> m
foldMapExpression f = execWriter . transformExpression id id (accumulate f)

-- | Accumulates a monoid over all expressions.
foldMapExpressions :: Monoid m => (Expression a b c d -> m) -> Program a b c d -> m
foldMapExpressions f = execWriter . transformProgram id id return (accumulate f) return return

accumulate :: Monoid m => (a -> m) -> a -> Writer m a
accumulate f a = tell (f a) >> return a

-- | Maps over all expression metadata in the abstract syntax tree.
mapExpressionMetas :: (c -> c') -> Program a b c d -> Program a b c' d
mapExpressionMetas f = runIdentity . transformProgram f id return return return return

-- | Maps over all node metadata in the abstract syntax tree.
mapNodeMetas :: (d -> d') -> Program a b c d -> Program a b c d'
mapNodeMetas g = runIdentity . transformProgram id g return return return return

-- | Powerful internal function to transform a program that is used to build up all the exported functions.
transformProgram :: Monad m =>
                    (c -> c')                                                -- ^ Expression metadata transformer
                    -> (d -> d')                                             -- ^ Node metadata transformer
                    -> (Declaration a b c' d' -> m (Declaration a b c' d'))  -- ^ Declaration transformer
                    -> (Expression a b c' d' -> m (Expression a b c' d'))    -- ^ Expression transformer
                    -> (Signature a b c' d' -> m (Signature a b c' d'))      -- ^ Signature transformer
                    -> (TypedVariable b d' -> m (TypedVariable b d'))        -- ^ TypedVariale transformer
                    -> Program a b c d                                       -- ^ Input program
                    -> m (Program a b c' d')                                 -- ^ Output program
transformProgram expMetaTrans nodeMetaTrans declTrans expTrans sigTrans tVarTrans (Program decls meta) =
  Program
    <$> mapM (transformDeclaration expMetaTrans nodeMetaTrans declTrans expTrans sigTrans tVarTrans) decls
    <*> pure (nodeMetaTrans meta)

-- | Powerful internal function to transform a declaration that is used to build up all the exported functions.
transformDeclaration :: Monad m =>
                        (c -> c')                                                -- ^ Expression metadata transformer
                        -> (d -> d')                                             -- ^ Node metadata transformer
                        -> (Declaration a b c' d' -> m (Declaration a b c' d'))  -- ^ Declaration transformer
                        -> (Expression a b c' d' -> m (Expression a b c' d'))    -- ^ Expression transformer
                        -> (Signature a b c' d' -> m (Signature a b c' d'))      -- ^ Signature transformer
                        -> (TypedVariable b d' -> m (TypedVariable b d'))        -- ^ TypedVariale transformer
                        -> Declaration a b c d                                   -- ^ Input declaration
                        -> m (Declaration a b c' d')                             -- ^ Output declaration
transformDeclaration expMetaTrans nodeMetaTrans declTrans expTrans sigTrans tVarTrans decl = declTrans =<< transformedDecl
  where transformedDecl = transformedDeclWithoutSigAndMeta <*> transformedSig <*> pure transformedMeta
        transformedSig = transformSignature expMetaTrans nodeMetaTrans expTrans sigTrans tVarTrans $ signature decl
        transformedMeta = nodeMetaTrans $ declMeta decl
        transformedDeclWithoutSigAndMeta = case decl of
          (Function _ body _) -> do
            body' <- transformExpression expMetaTrans nodeMetaTrans expTrans body
            return $ flip Function body'
          Extern{}            -> return Extern
  
-- | Powerful internal function to transform a signature that is used to build up all the exported functions.
transformSignature :: Monad m =>
                      (c -> c')                                              -- ^ Expression metadata transformer
                      -> (d -> d')                                           -- ^ Node metadata transformer
                      -> (Expression a b c' d' -> m (Expression a b c' d'))  -- ^ Expression transformer
                      -> (Signature a b c' d' -> m (Signature a b c' d'))    -- ^ Signature transformer
                      -> (TypedVariable b d' -> m (TypedVariable b d'))      -- ^ TypedVariale transformer
                      -> Signature a b c d                                   -- ^ Input signature
                      -> m (Signature a b c' d')                             -- ^ Output signature
transformSignature expMetaTrans nodeMetaTrans expTrans sigTrans tVarTrans (Signature isPure name args retType precs posts meta) =
  sigTrans =<< Signature isPure name
    <$> mapM (transformTypedVariable nodeMetaTrans tVarTrans) args
    <*> pure retType
    <*> transformConds precs
    <*> transformConds posts
    <*> pure (second nodeMetaTrans $ meta)
  where transformConds = mapM transformCond
        transformCond = transformExpression expMetaTrans nodeMetaTrans expTrans

transformTypedVariable :: Monad m =>
                          (d -> d')                                          -- ^ Node metadata transformer
                          -> (TypedVariable b d' -> m (TypedVariable b d'))  -- ^ TypedVariale transformer
                          -> TypedVariable b d                               -- ^ Input typed variable
                          -> m (TypedVariable b d')                          -- ^ Output typed variable
transformTypedVariable nodeMetaTrans tVarTrans (TypedVariable name typ meta) = tVarTrans $ TypedVariable name typ $ second nodeMetaTrans $ meta

-- | Powerful internal function to transform an expression that is used to build up all the exported functions.
transformExpression :: Monad m =>
                       (c -> c')                                              -- ^ Expression metadata transformer
                       -> (d -> d')                                           -- ^ Node metadata transformer
                       -> (Expression a b c' d' -> m (Expression a b c' d'))  -- ^ Expression transformer
                       -> Expression a b c d                                  -- ^ Input expression
                       -> m (Expression a b c' d')                            -- ^ Output expression
transformExpression expMetaTrans nodeMetaTrans expTrans exp = expTrans =<< transformedExp
    where transformSubExp = transformExpression expMetaTrans nodeMetaTrans expTrans
          transformedExp = transformedExpWithoutMeta <*> pure transformedMeta
          transformedMeta = first expMetaTrans $ second nodeMetaTrans $ expMeta exp
          transformedExpWithoutMeta = case exp of
            BinaryOperation op left right _  -> BinaryOperation op <$> transformSubExp left <*> transformSubExp right
            UnaryOperation op exp _          -> UnaryOperation op <$> transformSubExp exp
            Conditional cond ifExp elseExp _ -> Conditional <$> transformSubExp cond <*> transformSubExp ifExp <*> transformSubExp elseExp
            Call name args callMeta _        -> Call name <$> mapM transformSubExp args <*> pure callMeta
            Block stmts exp _                -> Block <$> mapM transformSubExp stmts <*> transformSubExp exp
            While cond body _                -> While <$> transformSubExp cond <*> transformSubExp body
            Variable name varMeta _          -> return $ Variable name varMeta
            Boolean b _                      -> return $ Boolean b
            Integer n _                      -> return $ Integer n
            Double d _                       -> return $ Double d
            Unit _                           -> return $ Unit
