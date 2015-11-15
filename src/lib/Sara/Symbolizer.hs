{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Sara.Symbolizer ( FunctionKey(..)
                       , functionKey
                       , callFunctionKey
                       , symbolize ) where

import Sara.Syntax
import Sara.Types
import Sara.Meta
import Sara.AstUtils
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.State
import Control.Monad.State.Class as S
import Data.Bifunctor
import qualified Sara.Builtins as B
import qualified Data.Map.Strict as M

data FunctionKey =
  FunctionKey { funcName :: Name, funcType :: [Type] }
  deriving (Eq, Ord, Show)

type FunctionMap = M.Map FunctionKey (SymbolizerSignature, FunctionMeta)
type VariableMap = M.Map Name (SymbolizerTypedVariable, VariableMeta)
type IdMap = M.Map Name Id

data SymbolizerContext
  = SymbolizerContext { funcs :: FunctionMap
                      , vars :: VariableMap }
    deriving (Eq, Ord, Show)

functionKey :: Signature a b c d -> FunctionKey
functionKey Signature{..} = FunctionKey sigName $ map varType args

lolTyp :: Expression a b TypMeta d -> Type
lolTyp = typTyp . expressionMeta

callFunctionKey :: Expression a b TypMeta d -> FunctionKey
callFunctionKey Call{..} = FunctionKey expName (map lolTyp expArgs)
callFunctionKey _        = error "Can only get the call function key for Call expressions."

symbolizeFunctions :: SymbolizerProgram -> (SymbolizerProgram, FunctionMap)
symbolizeFunctions prog = runWriter (evalStateT (mapMSignatures symbolizeSignature prog) M.empty)
  where symbolizeSignature :: (MonadState IdMap m, MonadWriter FunctionMap m) => SymbolizerSignature -> m SymbolizerSignature
        symbolizeSignature sig@Signature{..} = do
          sym <- getNewFunctionSymbol sig
          tell $ M.singleton (functionKey sig) (sig, sym)
          let sigMeta' = (first $ const sym) $ sigMeta
          return sig{ sigMeta = sigMeta' }

getNewFunctionSymbol :: MonadState IdMap m => Signature a b c d -> m FunctionMeta
getNewFunctionSymbol Signature{..} = getNewSymbol (FunctionMeta isPure (map varType args) retType) sigName

getNewVariableSymbol :: MonadState IdMap m => TypedVariable b d -> m VariableMeta
getNewVariableSymbol TypedVariable{..} = getNewSymbol (VariableMeta varType) varName

getNewSymbol :: MonadState IdMap m => (Name -> Id -> e) -> Name -> m e
getNewSymbol f name = do
  existing <- S.gets $ M.lookup name
  let id = case existing of
        Just n  -> n + 1
        Nothing -> 0
  S.modify $ M.insert name id
  return $ f name id

symbolizeTypedVars :: SymbolizerProgram -> SymbolizerProgram
symbolizeTypedVars prog = evalState (mapMTypedVariables symbolizeTypedVar prog) M.empty
  where symbolizeTypedVar :: MonadState IdMap m => SymbolizerTypedVariable -> m SymbolizerTypedVariable
        symbolizeTypedVar v@TypedVariable{..} = do
          sym <- getNewVariableSymbol v
          let varMeta' = (first $ const sym) $ varMeta
          return $ v{ varMeta = varMeta' }

addUndefinedSymbols :: TypeCheckerProgram -> SymbolizerProgram
addUndefinedSymbols = mapVariableMetas (const $ error "Accessed undefined variable metadata.")
                      . mapFunctionMetas (const $ error "Accessed undefined function metadata.")

resultVar :: SymbolizerSignature -> SymbolizerTypedVariable
resultVar Signature{..} = TypedVariable (B.name B.Result) retType (BuiltinVar retType B.Result, snd sigMeta)

symbolize :: TypeCheckerProgram -> SymbolizerProgram
symbolize prog = runReader
                 (weirdTransformExpressions tVarContextTrans symbolizeExp resultVar prog')
                 (SymbolizerContext functionMap M.empty)
  where (prog', functionMap) = symbolizeFunctions $ symbolizeTypedVars $ addUndefinedSymbols prog
        tVarContextTrans tVar = local $ addVar tVar
        addVar :: SymbolizerTypedVariable -> SymbolizerContext -> SymbolizerContext
        addVar v@TypedVariable{..} context = context{ vars = vars' }
          where vars' = M.insert (varName) (v, fst varMeta) $ vars context
        symbolizeExp :: MonadReader SymbolizerContext m => SymbolizerExpression -> m SymbolizerExpression
        symbolizeExp c@Call{}                           = do
          funcs' <- asks funcs
          let callKey = callFunctionKey c
          let expCallMeta' = case callKey `M.lookup` funcs' of
                Just (_, meta) -> meta
                Nothing        -> error $ "Function " ++ show callKey ++ " not found in symbol table " ++ show funcs' ++ "."
          return c{ expCallMeta = expCallMeta' }
        symbolizeExp v@Variable{ expName = name }       = do
          vars' <- asks vars
          let expVarMeta' = case name `M.lookup` vars' of
                Just (_, meta) -> meta
                Nothing        -> error $ "Variable " ++ name ++ " not found in symbol table " ++ show vars' ++ "."
          return v{ expVarMeta = expVarMeta' }
        symbolizeExp exp                                     = return exp
