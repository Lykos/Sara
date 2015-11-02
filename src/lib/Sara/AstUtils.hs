{-# LANGUAGE RankNTypes #-}

module Sara.AstUtils ( mapFunctionMetas
                     , mapVariableMetas
                     , mapExpressionMetas
                     , mapNodeMetas
                     , mapMExpression_
                     , mapMExpressions_
                     , mapMSignatures_
                     , mapMDeclarations_
                     , mapMSignatures
                     , mapMExpressions
                     , weirdTransformSymbols
                     , weirdTransformExpressions
                     , foldMapSignatures
                     , foldMapExpression
                     , foldMapExpressions ) where

import Data.Bifunctor
import Sara.Syntax
import Sara.Utils
import Control.Monad.Writer
import Control.Monad.Identity

-- | Maps over all function metadata.
mapFunctionMetas :: (a -> a') -> Program a b c d -> Program a' b c d
mapFunctionMetas f = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer f id id id nullTVarContextTrans return return return return

-- | Maps over all variable metadata.
mapVariableMetas :: (b -> b') -> Program a b c d -> Program a b' c d
mapVariableMetas f = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer id f id id nullTVarContextTrans return return return return

-- | Transformation that can transform all variable and function symbols.
-- The variable and function metadata has to be redefined, it is set to undefined initially.
weirdTransformSymbols :: Monad m =>
                         (forall x . TypedVariable b d -> m (m x -> m x))       -- ^ Context transformer based on a typed variable
                         -> (Expression a' b' c d -> m (Expression a' b' c d))  -- ^ Expression transformer
                         -> (Signature a' b' c d -> m (Signature a' b' c d))    -- ^ Signature transformer
                         -> (TypedVariable b' d -> m (TypedVariable b' d))      -- ^ TypedVariable transformer
                         -> Program a b c d                                     -- ^ Input program
                         -> m (Program a' b' c d)                               -- ^ Output program
weirdTransformSymbols tVarExpTrans expTrans sigTrans tVarTrans = transformProgramInternal transformer
  where transformer = AstTransformer (const undefined) (const undefined) id id tVarExpTrans return expTrans sigTrans tVarTrans

-- | Performs a transformation on all expressions in the AST.
-- The expression metadata has to be redefined, it is set to undefined initially.
-- The second argument can be used to do a context transformation based on an encountered typed variable that is valid for the current scope.
weirdTransformExpressions :: Monad m =>
                             (forall x . TypedVariable b d -> m x -> m x)         -- ^ Context transformer based on a typed variable
                             -> (Expression a b c' d -> m (Expression a b c' d))  -- ^ Expression transformer
                             -> Program a b c d                                   -- ^ Input program
                             -> m (Program a b c' d)                              -- ^ Output program
weirdTransformExpressions tVarExpTrans transExp = transformProgramInternal transformer
  where transformer = AstTransformer id id (const undefined) id tVarExpTrans' return transExp return return
        tVarExpTrans' v = return $ tVarExpTrans v

-- | Monadic map over all signatures.
mapMSignatures :: Monad m => (Signature a b c d -> m (Signature a b c d)) -> Program a b c d -> m (Program a b c d)
mapMSignatures sigTrans = transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans return return sigTrans return

-- | Monadic map over all signatures and discard the result.
mapMSignatures_ :: Monad m => (Signature a b c d -> m ()) -> Program a b c d -> m ()
mapMSignatures_ sigTrans prog = mapMSignatures sigTrans' prog >> return ()
  where sigTrans' sig = sigTrans sig >> return sig

-- | Monadic map over all expressions
mapMExpressions :: Monad m => (Expression a b c d -> m (Expression a b c d)) -> Program a b c d -> m (Program a b c d)
mapMExpressions expTrans = transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans return expTrans return return

-- | Monadic map over all expressions and discard the result.
mapMExpressions_ :: Monad m => (Expression a b c d -> m ()) -> Program a b c d -> m ()
mapMExpressions_ expTrans prog = mapMExpressions expTrans' prog >> return ()
  where expTrans' exp = expTrans exp >> return exp

-- | Accumulates a monoid over all signatures
foldMapSignatures :: Monoid m => (Signature a b c d -> m) -> Program a b c d -> m
foldMapSignatures f = execWriter . transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans return return (accumulate f) return

-- | Monadic map over one expression and its subexpressions and discard the result.
mapMExpression_ :: Monad m => (Expression a b c d -> m ()) -> Expression a b c d -> m ()
mapMExpression_ expTrans exp = transformExpressionInternal transformer exp >> return ()
  where transformer = AstTransformer id id id id nullTVarContextTrans undefined expTrans' undefined undefined
        expTrans' exp = expTrans exp >> return exp

-- | Monadic map over all declarations and discard the result.
mapMDeclarations_ :: Monad m => (Declaration a b c d -> m ()) -> Program a b c d -> m ()
mapMDeclarations_ declTrans prog = transformProgramInternal transformer prog >> return ()
  where transformer = AstTransformer id id id id nullTVarContextTrans declTrans' return return return
        declTrans' decl = declTrans decl >> return decl

-- | Accumulates a monoid over an expression.
foldMapExpression :: Monoid m => (Expression a b c d -> m) -> Expression a b c d -> m
foldMapExpression f = execWriter . transformExpressionInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans undefined (accumulate f) undefined undefined

-- | Accumulates a monoid over all expressions.
foldMapExpressions :: Monoid m => (Expression a b c d -> m) -> Program a b c d -> m
foldMapExpressions f = execWriter . transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans return (accumulate f) return return

accumulate :: Monoid m => (a -> m) -> a -> Writer m a
accumulate f a = tell (f a) >> return a

-- | Maps over all expression metadata in the abstract syntax tree.
mapExpressionMetas :: (c -> c') -> Program a b c d -> Program a b c' d
mapExpressionMetas f = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer id id f id nullTVarContextTrans return return return return

-- | Maps over all node metadata in the abstract syntax tree.
mapNodeMetas :: (d -> d') -> Program a b c d -> Program a b c d'
mapNodeMetas g = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer id id id g nullTVarContextTrans return return return return

nullTVarContextTrans :: Monad m => x -> m (m y -> m y)
nullTVarContextTrans = const $ return id

data AstTransformer a b c d a' b' c' d' m
  = AstTransformer { funcMetaTrans :: a -> a'                                               -- ^ Function metadata transformer
                   , varMetaTrans :: b -> b'                                                -- ^ Variable metadata transformer
                   , expMetaTrans :: c -> c'                                                -- ^ Expression metadata transformer
                   , nodeMetaTrans :: d -> d'                                               -- ^ Node metadata transformer
                   , tVarContextTrans :: forall x . TypedVariable b d -> m (m x -> m x)     -- ^ Context transformation based on a typed variable
                   , declTrans :: (Declaration a' b' c' d' -> m (Declaration a' b' c' d'))  -- ^ Declaration transformer
                   , expTrans :: (Expression a' b' c' d' -> m (Expression a' b' c' d'))     -- ^ Expression transformer
                   , sigTrans :: (Signature a' b' c' d' -> m (Signature a' b' c' d'))       -- ^ Signature transformer
                   , tVarTrans :: (TypedVariable b' d' -> m (TypedVariable b' d'))          -- ^ TypedVariable transformer
                   }

-- | Powerful internal function to transform a program that is used to build up all the exported functions.
transformProgramInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> Program a b c d -> m (Program a' b' c' d')
transformProgramInternal transformer (Program decls meta) =
  Program
    <$> mapM (transformDeclarationInternal transformer) decls
    <*> pure (nodeMetaTrans transformer meta)

-- | Powerful internal function to transform a declaration that is used to build up all the exported functions.
transformDeclarationInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> Declaration a b c d -> m (Declaration a' b' c' d')
transformDeclarationInternal transformer decl = foldlM (tVarContextTrans transformer) tVars $ declTrans transformer =<< transformedDecl
  where tVars = args $ signature decl
        transformedDecl = transformedDeclWithoutSigAndMeta <*> transformedSig <*> pure transformedMeta
        transformedSig = transformSignatureInternal transformer $ signature decl
        transformedMeta = nodeMetaTrans transformer $ declMeta decl
        transformedDeclWithoutSigAndMeta = case decl of
          (Function _ body _) -> flip Function <$> transformExpressionInternal transformer body
          Extern{}            -> return Extern

-- | Powerful internal function to transform a signature that is used to build up all the exported functions.
transformSignatureInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> Signature a b c d -> m (Signature a' b' c' d')
transformSignatureInternal transformer (Signature isPure name args retType precs posts meta) =
  sigTrans transformer =<< Signature isPure name
    <$> mapM (transformTypedVariableInternal transformer) args
    <*> pure retType
    <*> transformConds precs
    <*> transformConds posts
    <*> pure transformedMeta
  where transformConds = mapM transformCond
        transformCond = transformExpressionInternal transformer
        transformedMeta = first (funcMetaTrans transformer) $ second (nodeMetaTrans transformer) $ meta

transformTypedVariableInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> TypedVariable b d -> m (TypedVariable b' d')
transformTypedVariableInternal transformer (TypedVariable name typ meta) =
  tVarTrans transformer $ TypedVariable name typ $ transformedMeta
  where transformedMeta = first (varMetaTrans transformer) $ second (nodeMetaTrans transformer) $ meta

-- | Powerful internal function to transform an expression that is used to build up all the exported functions.
transformExpressionInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> Expression a b c d -> m (Expression a' b' c' d')
transformExpressionInternal transformer exp = expTrans transformer =<< transformedExp
    where transformSubExp = transformExpressionInternal transformer
          transformedExp = transformedExpWithoutMeta <*> pure transformedMeta
          transformedMeta = first (expMetaTrans transformer) $ second (nodeMetaTrans transformer) $ expMeta exp
          transformedExpWithoutMeta = case exp of
            BinaryOperation op left right _  -> BinaryOperation op <$> transformSubExp left <*> transformSubExp right
            UnaryOperation op exp _          -> UnaryOperation op <$> transformSubExp exp
            Conditional cond ifExp elseExp _ -> Conditional <$> transformSubExp cond <*> transformSubExp ifExp <*> transformSubExp elseExp
            Call name args callMeta _        -> Call name <$> mapM transformSubExp args <*> pure (funcMetaTrans transformer $ callMeta)
            Block stmts exp _                -> Block <$> mapM transformSubExp stmts <*> transformSubExp exp
            While cond body _                -> While <$> transformSubExp cond <*> transformSubExp body
            Variable name varMeta _          -> return $ Variable name (varMetaTrans transformer $ varMeta)
            Boolean b _                      -> return $ Boolean b
            Integer n _                      -> return $ Integer n
            Double d _                       -> return $ Double d
            Unit _                           -> return $ Unit
