module Sara.TypeChecker ( checkWithoutMain
                        , checkWithMain ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bifunctor
import qualified Data.Map.Strict as M

import qualified Sara.Types as T
import qualified Sara.Syntax as S
import Sara.Meta
import Sara.Syntax
import Sara.Types
import Sara.Operators
import Sara.AstUtils
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
  where checkRetType (Function sig body _) = when (bodyType /= sigType) (invalidRetType sigType bodyType (expressionPos body))
          where bodyType = expressionTyp body
                sigType = retType sig
        checkRetType Extern{}              = return ()

checkWithoutMain :: ParserProgram -> ErrorOr SymbolizerProgram
checkWithoutMain prog = C.checkWithoutMain prog >> typeCheckProgram prog

checkWithMain :: ParserProgram -> ErrorOr SymbolizerProgram
checkWithMain prog = C.checkWithMain prog >> typeCheckProgram prog

typeCheckProgram :: ParserProgram -> ErrorOr SymbolizerProgram
typeCheckProgram p = do
  funcs <- functions p
  p' <- typeCheckExpressions funcs p
  checkReturnTypes p'
  let p'' = symbolize p'
  checkPureness p''
  return p''

functions :: ParserProgram -> ErrorOr FunctionMap
functions prog = execStateT (mapMSignatures_ addSignature prog) M.empty
  where addSignature sig = do
          let f@(FunctionKey name argTypes) = functionKey sig
          oldFunc <- gets (M.lookup f)
          case oldFunc of
            Just sig' -> lift $ redeclaredFunction name argTypes (signaturePos sig') (signaturePos sig)
            Nothing   -> return ()
          modify $ insertFunction sig

insertFunction :: ParserSignature -> FunctionMap -> FunctionMap
insertFunction sig = M.insert (functionKey sig) sig

typeCheckExpressions :: FunctionMap -> ParserProgram -> ErrorOr TypeCheckerProgram
typeCheckExpressions funcMap program =
  runReaderT (weirdTransformExpressions typeCheckSingleExpression tVarContextTrans program) (TypeCheckerContext funcMap M.empty)
  where tVarContextTrans tVar = local (addVar tVar)
        addVar tVar context = context{ vars = vars' }
          where vars' = M.insert (varName tVar) tVar $ vars context
        typeCheckSingleExpression exp = case exp of
          S.Boolean{}                        -> typed T.Boolean
          S.Integer{}                        -> typed T.Integer
          S.Double{}                         -> typed T.Double
          S.Unit{}                           -> typed T.Unit
          UnaryOperation op subExp _         -> typed =<< unOpType op (expressionTyp subExp)
          BinaryOperation op left right _    -> typed =<< binOpType op (expressionTyp left) (expressionTyp right)
          Variable name _ _                  -> typed =<< varType name
          Call name args _ _                 -> typed =<< funcType name (map expressionTyp args)
          Conditional cond thenExp elseExp _ -> typed =<< condType (expressionTyp cond) (expressionTyp thenExp) (expressionTyp elseExp) (expressionPos cond)
          Block _ exp _                      -> typed (expressionTyp exp)
          While cond _ _                     -> typed =<< whileType (expressionTyp cond) (expressionPos cond)
          where pos = expressionPos exp
                condType T.Boolean thenType elseType _ | thenType == elseType = return thenType
                                                       | otherwise            = lift $ mismatchingCondTypes thenType elseType pos
                condType condTyp _ _ condPos                                  = lift $ invalidCondType condTyp condPos
                whileType T.Boolean _     = return T.Unit
                whileType condTyp condPos = lift $ invalidCondType condTyp condPos
                typed :: Type -> ReaderT TypeCheckerContext (ExceptT Error Identity) TypeCheckerExpression
                typed t = return exp{ expMeta = meta' }
                  where meta' :: (ExpressionMeta, NodeMeta)
                        meta' = first (const $ ExpressionMeta t) (expMeta exp)
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
