module Sara.TypeChecker ( checkWithoutMain
                        , checkWithMain ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Trans.Reader
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
import Sara.Symbols

data TypeCheckerContext = TypeCheckerContext { funcs :: FunctionMap
                                             , vars :: VariableMap }

checkReturnTypes :: TypeCheckerProgram -> ErrorOr ()
checkReturnTypes = mapMDeclarations_ checkRetType
  where checkRetType (Function sig body _) = when (bodyType /= sigType) (invalidRetType sigType bodyType (expressionPos body))
          where bodyType = expressionTyp body
                sigType = retType sig
        checkRetType Extern{}              = return ()

checkWithoutMain :: ParserProgram -> ErrorOr TypeCheckerProgram
checkWithoutMain prog = C.checkWithoutMain prog >> typeCheckProgram prog

checkWithMain :: ParserProgram -> ErrorOr TypeCheckerProgram
checkWithMain prog = C.checkWithMain prog >> typeCheckProgram prog

typeCheckProgram :: ParserProgram -> ErrorOr TypeCheckerProgram
typeCheckProgram p = do
  funcs <- functions p
  p' <- typeCheckExpressions funcs p
  checkReturnTypes p'
  checkPureness funcs p'
  return p'

functions :: ParserProgram -> ErrorOr FunctionMap
functions = functionsFromDecls . program

functionsFromDecls :: [ParserDeclaration] -> ErrorOr FunctionMap
functionsFromDecls = foldl addOneFunction (return M.empty)

addOneFunction :: ErrorOr FunctionMap -> ParserDeclaration -> ErrorOr FunctionMap
addOneFunction funcs decl = do
  let sig = signature decl
  let pos = declarationPos decl
  funcs' <- funcs
  let f@(FunctionKey name argTypes) = functionKey sig
  case f `M.lookup` funcs' of
    Just sig' -> redeclaredFunction name argTypes (signaturePos sig') pos
    Nothing   -> return ()
  return $ insertFunction sig funcs'

insertFunction :: ParserSignature -> FunctionMap -> FunctionMap
insertFunction sig = M.insert (functionKey sig) sig

functionKey :: Signature a b c d -> FunctionKey
functionKey Signature{ sigName = name, args = args } = FunctionKey name $ map varType args

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
                  where meta' :: (TypeCheckerExpressionMeta, ParserNodeMeta)
                        meta' = first (const $ TypeCheckerExpressionMeta t) (expMeta exp)
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
