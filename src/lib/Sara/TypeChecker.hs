{-# LANGUAGE RecordWildCards #-}

module Sara.TypeChecker ( checkWithoutMain
                        , checkWithMain ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bifunctor
import qualified Sara.Builtins as B
import qualified Data.Map.Strict as M

import Sara.Meta
import Sara.Syntax as S
import Sara.Types as T
import Sara.Operators
import Sara.AstUtils
import Sara.Utils
import Sara.Errors
import qualified Sara.Checker as C
import Sara.PureChecker
import Sara.Symbolizer

type FunctionMap = M.Map FunctionKey ParserSignature
type VariableMap = M.Map Name ParserTypedVariable

data TypeCheckerContext = TypeCheckerContext { funcs :: FunctionMap
                                             , vars :: VariableMap }

checkReturnTypes :: TypeCheckerProgram -> ErrorOr ()
checkReturnTypes = mapMDeclarations_ checkRetType
  where checkRetType (S.Function sig body _) = do
          let bodyType = expressionTyp' body
          let sigType = retType sig
          when (bodyType /= sigType) (invalidRetType sigType bodyType (expressionPos body))
          typeCheckCondTypes $ S.preconditions sig
          typeCheckCondTypes $ S.postconditions sig
        checkRetType Extern{}              = return ()

checkWithoutMain :: ParserProgram -> ErrorOr PureCheckerProgram
checkWithoutMain prog = C.checkWithoutMain prog >> typeCheckProgram prog

checkWithMain :: ParserProgram -> ErrorOr PureCheckerProgram
checkWithMain prog = C.checkWithMain prog >> typeCheckProgram prog

typeCheckProgram :: ParserProgram -> ErrorOr PureCheckerProgram
typeCheckProgram p = do
  funcs <- functions p
  p' <- typeCheckExpressions funcs p
  checkReturnTypes p'
  let p'' = symbolize p'
  checkPureness p''

functions :: ParserProgram -> ErrorOr FunctionMap
functions prog = execStateT (mapMSignatures_ addSignature prog) M.empty
  where addSignature sig = do
          let f@(FunctionKey name argTypes) = functionKey sig
          oldFunc <- gets (M.lookup f)
          case oldFunc of
            Just sig' -> lift $ redeclaredFunction (functionOrMethod (isPure sig') name) argTypes (signaturePos sig') (signaturePos sig)
            Nothing   -> return ()
          modify $ insertFunction sig

insertFunction :: ParserSignature -> FunctionMap -> FunctionMap
insertFunction sig = M.insert (functionKey sig) sig

-- | Checks that the type of a condition is a boolean.
typeCheckCondType :: TypeCheckerExpression -> ErrorOr ()
typeCheckCondType cond = let condPos = expressionPos cond in
  case expressionTyp' cond of
    T.Boolean -> return ()
    condTyp   -> invalidCondType condTyp condPos

typeCheckCondTypes :: [TypeCheckerExpression] -> ErrorOr ()
typeCheckCondTypes = mapM_ typeCheckCondType

resultVar :: TypeCheckerSignature -> TypeCheckerTypedVariable
resultVar Signature{..} = TypedVariable (B.name B.Result) retType ((), snd sigMeta)

typeCheckExpressions :: FunctionMap -> ParserProgram -> ErrorOr TypeCheckerProgram
typeCheckExpressions funcMap program =
  runReaderT (weirdTransformExpressions tVarContextTrans typeCheckSingleExpression resultVar program') (TypeCheckerContext funcMap M.empty)
  where program' :: TypeCheckerProgram
        program' = mapExpressionMetas (const $ error "Accessed undefined expression metadata.") program
        tVarContextTrans tVar = local (addVar tVar)
        addVar tVar context = context{ vars = vars' }
          where vars' = M.insert (varName tVar) tVar $ vars context
        typeCheckSingleExpression exp = case exp of
          S.Boolean{}                        -> typed T.Boolean
          S.Integer{}                        -> typed T.Integer
          S.Double{}                         -> typed T.Double
          S.Unit{}                           -> typed T.Unit
          UnaryOperation op subExp _         -> typed =<< unOpType op (expressionTyp' subExp)
          BinaryOperation op left right _    -> typed =<< binOpType op (expressionTyp' left) (expressionTyp' right)
          Variable name _ _                  -> typed =<< varType name
          Call name args _ _                 -> typed =<< funcType name (map expressionTyp' args)
          Conditional cond thenExp elseExp _ -> typed
                                                =<< conditionalType (expressionTyp' thenExp) (expressionTyp' elseExp)
                                                << lift (typeCheckCondType cond)
          Block _ exp _                      -> typed (expressionTyp' exp)
          While invs cond _ _                -> typed T.Unit
                                                << lift (typeCheckCondType cond)
                                                << lift (typeCheckCondTypes invs)
          Assertion _ cond _                 -> typed T.Unit << lift (typeCheckCondType cond)
          where pos = expressionPos exp
                conditionalType thenType elseType | thenType == elseType = return thenType
                                                  | otherwise            = lift $ mismatchingCondTypes thenType elseType pos
                typed :: Type -> ReaderT TypeCheckerContext (ExceptT Error Identity) TypeCheckerExpression
                typed t = return exp{ expMeta = meta' }
                  where meta' :: (TypMeta, NodeMeta)
                        meta' = first (const $ TypMeta t) (expMeta exp)
                varType :: Name -> ReaderT TypeCheckerContext (ExceptT Error Identity) Type
                varType name = do
                  vars' <- asks vars
                  case name `M.lookup` vars' of
                    Nothing -> lift $ unknownVariable name pos
                    Just t  -> return $ S.varType t
                funcType :: Name -> [Type] -> ReaderT TypeCheckerContext (ExceptT Error Identity) Type
                funcType name argTypes = do
                  funcs' <- asks funcs
                  case FunctionKey name argTypes `M.lookup` funcs' of
                    Nothing  -> lift $ unknownFunction name argTypes pos
                    Just sig -> return $ retType sig
                unOpType :: UnaryOperator -> Type -> ReaderT a (ExceptT Error Identity) Type
                unOpType op t = case TypedUnOp op t `M.lookup` typedUnOps of
                  Nothing -> lift $ unknownUnOp op t pos
                  Just t  -> return t
                binOpType :: BinaryOperator -> Type -> Type -> ReaderT a (ExceptT Error Identity) Type
                binOpType op s t = case TypedBinOp op s t `M.lookup` typedBinOps of
                  Nothing -> lift $ unknownBinOp op s t pos
                  Just t  -> return t
