module Sara.AstUtils ( mapNodeMetas
                     , mapExpressionMetas
                     , mapMExpression_
                     , mapMExpressions_
                     , mapMSignatures_
                     , mapMDeclarations_
                     , weirdTransformExpressions
                     , mapMSignatures
                     , mapMExpressions
                     , foldMapSignatures
                     , foldMapExpression
                     , foldMapExpressions ) where

import Data.Bifunctor
import Sara.Syntax
import Control.Monad.Writer
import Control.Monad.Identity

-- | Performs a transformation on all expressions in the AST.
-- The expression metadata has to be redefined, it is set to undefined initially.
-- The second argument can be used to do a context transformation based on an encountered typed variable that is valid for the current scope.
weirdTransformExpressions :: Monad m =>
                             (Expression a b c' d -> m (Expression a b c' d))                              -- ^ Expression transformer
                             -> (TypedVariable b d -> m (Expression a b c' d) -> m (Expression a b c' d))  -- ^ Context transformer based on a typed variable
                             -> Program a b c d                                                            -- ^ Input program
                             -> m (Program a b c' d)                                                       -- ^ Output program
weirdTransformExpressions transExp tVarExpTrans = transformProgramInternal transformer
  where transformer = AstTransformer (const undefined) id tVarExpTrans return transExp return return

-- | Monadic map over all signatures.
mapMSignatures :: Monad m => (Signature a b c d -> m (Signature a b c d)) -> Program a b c d -> m (Program a b c d)
mapMSignatures sigTrans = transformProgramInternal transformer
  where transformer = AstTransformer id id (const id ) return return sigTrans return

-- | Monadic map over all signatures and discard the result.
mapMSignatures_ :: Monad m => (Signature a b c d -> m ()) -> Program a b c d -> m ()
mapMSignatures_ sigTrans prog = mapMSignatures sigTrans' prog >> return ()
  where sigTrans' sig = sigTrans sig >> return sig

-- | Monadic map over all expressions
mapMExpressions :: Monad m => (Expression a b c d -> m (Expression a b c d)) -> Program a b c d -> m (Program a b c d)
mapMExpressions expTrans = transformProgramInternal transformer
  where transformer = AstTransformer id id (const id) return expTrans return return

-- | Monadic map over all expressions and discard the result.
mapMExpressions_ :: Monad m => (Expression a b c d -> m ()) -> Program a b c d -> m ()
mapMExpressions_ expTrans prog = mapMExpressions expTrans' prog >> return ()
  where expTrans' exp = expTrans exp >> return exp

-- | Accumulates a monoid over all signatures
foldMapSignatures :: Monoid m => (Signature a b c d -> m) -> Program a b c d -> m
foldMapSignatures f = execWriter . transformProgramInternal transformer
  where transformer = AstTransformer id id (const id) return return (accumulate f) return

-- | Monadic map over one expression and its subexpressions and discard the result.
mapMExpression_ :: Monad m => (Expression a b c d -> m ()) -> Expression a b c d -> m ()
mapMExpression_ expTrans exp = transformExpressionInternal transformer exp >> return ()
  where transformer = AstTransformer id id (const id) undefined expTrans' undefined undefined
        expTrans' exp = expTrans exp >> return exp

-- | Monadic map over all declarations and discard the result.
mapMDeclarations_ :: Monad m => (Declaration a b c d -> m ()) -> Program a b c d -> m ()
mapMDeclarations_ declTrans prog = transformProgramInternal transformer prog >> return ()
  where transformer = AstTransformer id id (const id) declTrans' return return return
        declTrans' decl = declTrans decl >> return decl

-- | Accumulates a monoid over an expression.
foldMapExpression :: Monoid m => (Expression a b c d -> m) -> Expression a b c d -> m
foldMapExpression f = execWriter . transformExpressionInternal transformer
  where transformer = AstTransformer id id (const id) undefined (accumulate f) undefined undefined

-- | Accumulates a monoid over all expressions.
foldMapExpressions :: Monoid m => (Expression a b c d -> m) -> Program a b c d -> m
foldMapExpressions f = execWriter . transformProgramInternal transformer
  where transformer = AstTransformer id id (const id) return (accumulate f) return return

accumulate :: Monoid m => (a -> m) -> a -> Writer m a
accumulate f a = tell (f a) >> return a

-- | Maps over all expression metadata in the abstract syntax tree.
mapExpressionMetas :: (c -> c') -> Program a b c d -> Program a b c' d
mapExpressionMetas f = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer f id (const id) return return return return

-- | Maps over all node metadata in the abstract syntax tree.
mapNodeMetas :: (d -> d') -> Program a b c d -> Program a b c d'
mapNodeMetas g = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer id g (const id) return return return return

data AstTransformer a b c d c' d' m
  = AstTransformer { expMetaTrans :: c -> c'                                            -- ^ Expression metadata transformer
                   , nodeMetaTrans :: d -> d'                                           -- ^ Node metadata transformer
                     -- | Context transformation based on a typed variable
                   , tVarContextTrans :: TypedVariable b d -> m (Expression a b c' d') -> m (Expression a b c' d')
                   , declTrans :: (Declaration a b c' d' -> m (Declaration a b c' d'))  -- ^ Declaration transformer
                   , expTrans :: (Expression a b c' d' -> m (Expression a b c' d'))     -- ^ Expression transformer
                   , sigTrans :: (Signature a b c' d' -> m (Signature a b c' d'))       -- ^ Signature transformer
                   , tVarTrans :: (TypedVariable b d' -> m (TypedVariable b d'))        -- ^ TypedVariable transformer
                   }

-- | Powerful internal function to transform a program that is used to build up all the exported functions.
transformProgramInternal :: Monad m => AstTransformer a b c d c' d' m -> Program a b c d -> m (Program a b c' d')
transformProgramInternal transformer (Program decls meta) =
  Program
    <$> mapM (transformDeclarationInternal transformer) decls
    <*> pure (nodeMetaTrans transformer meta)

-- | Powerful internal function to transform a declaration that is used to build up all the exported functions.
transformDeclarationInternal :: Monad m => AstTransformer a b c d c' d' m -> Declaration a b c d -> m (Declaration a b c' d')
transformDeclarationInternal transformer decl = declTrans transformer =<< transformedDecl
  where transformer' = transformer{ expTrans = expTrans' }
        expTrans' exp = foldr (tVarContextTrans transformer) (expTrans transformer exp) (args $ signature decl)
        transformedDecl = transformedDeclWithoutSigAndMeta <*> transformedSig <*> pure transformedMeta
        transformedSig = transformSignatureInternal transformer' $ signature decl
        transformedMeta = nodeMetaTrans transformer $ declMeta decl
        transformedDeclWithoutSigAndMeta = case decl of
          (Function _ body _) -> flip Function <$> transformExpressionInternal transformer' body
          Extern{}            -> return Extern
  
-- | Powerful internal function to transform a signature that is used to build up all the exported functions.
transformSignatureInternal :: Monad m => AstTransformer a b c d c' d' m -> Signature a b c d -> m (Signature a b c' d')
transformSignatureInternal transformer (Signature isPure name args retType precs posts meta) =
  sigTrans transformer =<< Signature isPure name
    <$> mapM (transformTypedVariableInternal transformer) args
    <*> pure retType
    <*> transformConds precs
    <*> transformConds posts
    <*> pure (second (nodeMetaTrans transformer) meta)
  where transformConds = mapM transformCond
        transformCond = transformExpressionInternal transformer

transformTypedVariableInternal :: Monad m => AstTransformer a b c d c' d' m -> TypedVariable b d -> m (TypedVariable b d')
transformTypedVariableInternal transformer (TypedVariable name typ meta) =
  tVarTrans transformer $ TypedVariable name typ $ second (nodeMetaTrans transformer) meta

-- | Powerful internal function to transform an expression that is used to build up all the exported functions.
transformExpressionInternal :: Monad m => AstTransformer a b c d c' d' m -> Expression a b c d -> m (Expression a b c' d')
transformExpressionInternal transformer exp = expTrans transformer =<< transformedExp
    where transformSubExp = transformExpressionInternal transformer
          transformedExp = transformedExpWithoutMeta <*> pure transformedMeta
          transformedMeta = first (expMetaTrans transformer) $ second (nodeMetaTrans transformer) $ expMeta exp
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
