{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sara.Ast.AstUtils ( mapFunctionMetas
                     , mapVariableMetas
                     , mapExpressionMetas
                     , mapNodeMetas
                     , mapExpressions
                     , mapMExpression_
                     , mapMExpressions_
                     , mapMTypedVariables
                     , mapMSignatures_
                     , mapMDeclarations_
                     , mapMSignatures
                     , mapMExpressions
                     , weirdTransformExpressions
                     , foldMapSignatures
                     , foldMapDeclarations
                     , foldMapExpression
                     , foldMapExpressions
                     , children ) where

import Data.Maybe
import Sara.Ast.Syntax
import Control.Monad.Writer
import Control.Monad.Identity

-- | Maps over all function metadata.
mapFunctionMetas :: (a -> a') -> Program a b c d -> Program a' b c d
mapFunctionMetas f = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer f id id id nullTVarContextTrans Nothing return return return return

-- | Maps over all variable metadata.
mapVariableMetas :: (b -> b') -> Program a b c d -> Program a b' c d
mapVariableMetas f = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer id f id id nullTVarContextTrans Nothing return return return return

-- | Maps over all expression metadata in the abstract syntax tree.
mapExpressionMetas :: (c -> c') -> Program a b c d -> Program a b c' d
mapExpressionMetas f = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer id id f id nullTVarContextTrans Nothing return return return return

-- | Maps over all node metadata in the abstract syntax tree.
mapNodeMetas :: (d -> d') -> Program a b c d -> Program a b c d'
mapNodeMetas g = runIdentity . transformProgramInternal transformer
  where transformer = AstTransformer id id id g nullTVarContextTrans Nothing return return return return

-- | Performs a transformation on all expressions in the AST.
-- The second argument can be used to do a context transformation based on an encountered typed variable that is valid for the current scope.
weirdTransformExpressions :: Monad m =>
                             (forall x . TypedVariable b d -> m x -> m x)       -- ^ Context transformer based on a typed variable
                             -> (Expression a b c d -> m (Expression a b c d))  -- ^ Expression transformer
                             -> (Signature a b c d -> TypedVariable b d)        -- ^ Special result variable that exists in all postconditions
                             -> Program a b c d                                 -- ^ Input program
                             -> m (Program a b c d)                             -- ^ Output program
weirdTransformExpressions tVarExpTrans transExp resultVar = transformProgramInternal transformer
  where transformer = AstTransformer id id id id tVarExpTrans (Just resultVar) return transExp return return

-- | Monadic map over all signatures.
mapMSignatures :: Monad m => (Signature a b c d -> m (Signature a b c d)) -> Program a b c d -> m (Program a b c d)
mapMSignatures sigTrans = transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing return return sigTrans return

-- | Monadic map over all signatures and discard the result.
mapMSignatures_ :: Monad m => (Signature a b c d -> m ()) -> Program a b c d -> m ()
mapMSignatures_ sigTrans prog = mapMSignatures sigTrans' prog >> return ()
  where sigTrans' sig = sigTrans sig >> return sig

-- | Monadic map over all expressions
mapMExpressions :: Monad m => (Expression a b c d -> m (Expression a b c d)) -> Program a b c d -> m (Program a b c d)
mapMExpressions expTrans = transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing return expTrans return return

-- | Map over all expressions
mapExpressions :: (Expression a b c d -> Expression a b c d) -> Program a b c d -> Program a b c d
mapExpressions f = runIdentity . mapMExpressions (Identity . f)

-- | Monadic map over all expressions and discard the result.
mapMExpressions_ :: Monad m => (Expression a b c d -> m ()) -> Program a b c d -> m ()
mapMExpressions_ expTrans prog = mapMExpressions expTrans' prog >> return ()
  where expTrans' exp = expTrans exp >> return exp

-- | Monadic map over all typed variables.
mapMTypedVariables :: Monad m => (TypedVariable b d -> m (TypedVariable b d)) -> Program a b c d -> m (Program a b c d)
mapMTypedVariables tVarTrans = transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing return return return tVarTrans

-- | Accumulates a monoid over all signatures
foldMapSignatures :: Monoid m => (Signature a b c d -> m) -> Program a b c d -> m
foldMapSignatures f = execWriter . transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing return return (accumulate f) return

-- | Monadic map over one expression and its subexpressions and discard the result.
mapMExpression_ :: Monad m => (Expression a b c d -> m ()) -> Expression a b c d -> m ()
mapMExpression_ expTrans exp = transformExpressionInternal transformer exp >> return ()
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing undefined expTrans' undefined undefined
        expTrans' exp = expTrans exp >> return exp

-- | Monadic map over all declarations and discard the result.
mapMDeclarations_ :: Monad m => (Declaration a b c d -> m ()) -> Program a b c d -> m ()
mapMDeclarations_ declTrans prog = transformProgramInternal transformer prog >> return ()
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing declTrans' return return return
        declTrans' decl = declTrans decl >> return decl

-- | Accumulates a monoid over an expression.
foldMapExpression :: Monoid m => (Expression a b c d -> m) -> Expression a b c d -> m
foldMapExpression f = execWriter . transformExpressionInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing undefined (accumulate f) undefined undefined

-- | Accumulates a monoid over all expressions.
foldMapExpressions :: Monoid m => (Expression a b c d -> m) -> Program a b c d -> m
foldMapExpressions f = execWriter . transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing return (accumulate f) return return

-- | Accumulates a monoid over all declarations.
foldMapDeclarations :: Monoid m => (Declaration a b c d -> m) -> Program a b c d -> m
foldMapDeclarations f = execWriter . transformProgramInternal transformer
  where transformer = AstTransformer id id id id nullTVarContextTrans Nothing (accumulate f) return return return

accumulate :: Monoid m => (a -> m) -> a -> Writer m a
accumulate f a = tell (f a) >> return a

nullTVarContextTrans :: Monad m => x -> m y -> m y
nullTVarContextTrans = const id

data AstTransformer a b c d a' b' c' d' m
  = AstTransformer { funcMetaTrans :: a -> a'                                               -- ^ Function metadata transformer
                   , varMetaTrans :: b -> b'                                                -- ^ Variable metadata transformer
                   , expMetaTrans :: c -> c'                                                -- ^ Expression metadata transformer
                   , nodeMetaTrans :: d -> d'                                               -- ^ Node metadata transformer
                   , tVarContextTrans :: forall x . TypedVariable b d -> m x -> m x         -- ^ Context transformation based on a typed variable
                   , resultVar :: Maybe (Signature a b c d -> TypedVariable b d)            -- ^ Special result variable that exists in all postconditions
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
transformDeclarationInternal transformer decl = foldl (flip $ tVarContextTrans transformer) (declTrans transformer =<< transformedDecl) tVars
  where tVars = args $ signature decl
        transformedDecl = transformedDeclWithoutSigAndMeta <*> transformedSig <*> pure transformedMeta
        transformedSig = transformSignatureInternal transformer $ signature decl
        transformedMeta = nodeMetaTrans transformer $ declMeta decl
        transformedDeclWithoutSigAndMeta = case decl of
          (Function _ body _) -> flip Function <$> transformExpressionInternal transformer body
          Extern{}            -> return Extern

-- | Powerful internal function to transform a signature that is used to build up all the exported functions.
transformSignatureInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> Signature a b c d -> m (Signature a' b' c' d')
transformSignatureInternal transformer sig@Signature{..} =
  sigTrans transformer =<< Signature isPure sigName
    <$> mapM (transformTypedVariableInternal transformer) args
    <*> pure retType
    <*> transformConds preconditions
    <*> transformedPosts
    <*> pure transformedSigMeta
    <*> pure transformedNodeMeta
  where transformConds = mapM transformCond
        transformCond = transformExpressionInternal transformer
        transformedSigMeta = funcMetaTrans transformer $ sigMeta
        transformedNodeMeta = nodeMetaTrans transformer $ sigNodeMeta
        transformedPosts = case resultVar transformer of
          Nothing -> transformConds postconditions
          Just v  -> (tVarContextTrans transformer) (v sig) $ transformConds postconditions

transformTypedVariableInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> TypedVariable b d -> m (TypedVariable b' d')
transformTypedVariableInternal transformer (TypedVariable name typ varMeta nodeMeta) =
  tVarTrans transformer $ TypedVariable name typ transformedVarMeta transformedNodeMeta
  where transformedVarMeta = varMetaTrans transformer varMeta
        transformedNodeMeta = nodeMetaTrans transformer nodeMeta

-- | Powerful internal function to transform an expression that is used to build up all the exported functions.
transformExpressionInternal :: Monad m => AstTransformer a b c d a' b' c' d' m -> Expression a b c d -> m (Expression a' b' c' d')
transformExpressionInternal transformer exp = expTrans transformer =<< transformedExp
    where transformSubExp = transformExpressionInternal transformer
          transformedExp = transformedExpWithoutMeta <*> pure transformedExpMeta <*> pure transformedNodeMeta
          transformedExpMeta = expMetaTrans transformer $ expMeta exp
          transformedNodeMeta = nodeMetaTrans transformer $ nodeMeta exp
          transformedExpWithoutMeta = case exp of
            BinaryOperation op left right _ _  -> BinaryOperation op <$> transformSubExp left <*> transformSubExp right
            UnaryOperation op exp _ _          -> UnaryOperation op <$> transformSubExp exp
            Conditional cond ifExp elseExp _ _ -> Conditional <$> transformSubExp cond <*> transformSubExp ifExp <*> transformSubExp elseExp
            Call name args callMeta _ _        -> Call name <$> mapM transformSubExp args <*> pure (funcMetaTrans transformer $ callMeta)
            Block stmts exp _ _                -> foldl (flip $ tVarContextTrans transformer) transformedBlock tVars
              where transformedBlock = Block <$> mapM transformSubExp stmts <*> transformSubExp exp
                    tVars = catMaybes $ map extractTVar stmts
                    extractTVar (VarDef t _ _ _ _) = Just t
                    extractTVar _                  = Nothing
            While invs cond body _ _           -> While <$> mapM transformSubExp invs <*> transformSubExp cond <*> transformSubExp body
            Variable name varMeta _ _          -> return $ Variable name (varMetaTrans transformer $ varMeta)
            Boolean b _ _                      -> return $ Boolean b
            Integer n _ _                      -> return $ Integer n
            Double d _ _                       -> return $ Double d
            Unit _ _                           -> return $ Unit
            Assertion k exp _ _                -> Assertion k <$> transformSubExp exp
            VarDef t v exp _ _                 -> VarDef <$> transformTypedVariableInternal transformer t <*> pure v <*> transformSubExp exp

-- | Returns the direct children of an expression.
children :: Expression a b c d -> [Expression a b c d]
children (BinaryOperation _ left right _ _)   = [left, right]
children (UnaryOperation _ exp _ _)           = [exp]
children (Conditional cond ifExp elseExp _ _) = [cond, ifExp, elseExp]
children (Call _ args _ _ _)                  = args
children (Block stmts exp _ _)                = stmts ++ [exp]
children (While invs cond body _ _)           = invs ++ [cond, body]
children Variable{}                           = []
children Boolean{}                            = []
children Integer{}                            = []
children Double{}                             = []
children Unit{}                               = []
children (Assertion _ exp _ _)                = [exp]
children (VarDef _ _ exp _ _)                 = [exp]
