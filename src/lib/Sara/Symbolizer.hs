{-# LANGUAGE FlexibleContexts #-}

module Sara.Symbolizer ( FunctionKey(..)
                       , functionKey
                       , callFunctionKey
                       , symbolize ) where

import Sara.Syntax
import Sara.Types
import Sara.Meta
import Sara.AstUtils
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.State
import Control.Monad.State.Class as S
import Data.Bifunctor
import qualified Data.Map.Strict as M

data FunctionKey =
  FunctionKey { funcName :: Name, funcType :: [Type] }
  deriving (Eq, Ord, Show)

type FunctionMap = M.Map FunctionKey (TypeCheckerSignature, FunctionMeta)
type VariableMap = M.Map Name (ParserTypedVariable, VariableMeta)
type IdMap = M.Map Name Id

data SymbolizerContext
  = SymbolizerContext { funcs :: FunctionMap
                      , vars :: VariableMap }
    deriving (Eq, Ord, Show)

functionKey :: Signature a b c d -> FunctionKey
functionKey Signature{ sigName = name, args = args } = FunctionKey name $ map varType args

callFunctionKey :: Expression a b ExpressionMeta d -> FunctionKey
callFunctionKey Call{ expName = name, expArgs = args } = FunctionKey name (map expressionTyp args)
callFunctionKey _                                      = error "Can only get the call function key for Call expressions."

functions :: TypeCheckerProgram -> FunctionMap
functions prog = execWriter (evalStateT (mapMSignatures_ addSignature prog) M.empty)
  where addSignature :: TypeCheckerSignature -> StateT IdMap (WriterT FunctionMap Identity) ()
        addSignature sig = do
          sym <- getNewFunctionSymbol sig
          tell $ M.singleton (functionKey sig) (sig, sym)

getNewFunctionSymbol :: MonadState IdMap s => Signature a b c d -> s FunctionMeta
getNewFunctionSymbol Signature{ sigName = name } = getNewSymbol FunctionMeta name

getNewVariableSymbol :: MonadState IdMap s => TypedVariable b d -> s VariableMeta
getNewVariableSymbol TypedVariable{ varName = name } = getNewSymbol VariableMeta name

getNewSymbol :: MonadState IdMap s => (Name -> Id -> e) -> Name -> s e
getNewSymbol f name = do
  existing <- S.gets $ M.lookup name
  let id = case existing of
        Just n  -> n + 1
        Nothing -> 0
  S.modify $ M.insert name id
  return $ f name id

symbolize :: TypeCheckerProgram -> SymbolizerProgram
symbolize prog = runReader
                 (evalStateT (weirdTransformSymbols tVarContextTrans symbolizeExp symbolizeSig symbolizeTypedVar prog) M.empty)
                 (SymbolizerContext (functions prog) M.empty)
  where tVarContextTrans tVar = do
          sym <- getNewVariableSymbol tVar
          return $ local (addVar tVar sym)
        addVar :: TypeCheckerTypedVariable -> VariableMeta -> SymbolizerContext -> SymbolizerContext
        addVar tVar sym context = context{ vars = vars' }
          where vars' = M.insert (varName tVar) (tVar, sym) $ vars context
        symbolizeExp :: SymbolizerExpression -> StateT IdMap (ReaderT SymbolizerContext Identity) SymbolizerExpression
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
        symbolizeSig sig@Signature{ sigMeta = sigMeta } = do
          funcs' <- asks funcs
          let funcKey = functionKey sig
          let meta = case funcKey `M.lookup` funcs' of
                Just (_, meta) -> meta
                Nothing        -> error $ "Function declaration " ++ show funcKey ++ " not found in symbol table " ++ show funcs' ++ "."
          let sigMeta' = (first $ const meta) $ sigMeta
          return sig{ sigMeta = sigMeta' }
        symbolizeTypedVar var@TypedVariable{ varName = name, varMeta = varMeta }  = do
          vars' <- asks vars
          let meta = case name `M.lookup` vars' of
                Just (_, meta) -> meta
                Nothing        -> error $ "Variable declaration " ++ name ++ " not found in symbol table " ++ show vars' ++ "."
          let varMeta' = (first $ const meta) $ varMeta
          return var{ varMeta = varMeta' }
