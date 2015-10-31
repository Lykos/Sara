module Sara.TypeChecker (
  checkWithoutMain
  , checkWithMain) where

import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Bifunctor

import qualified Data.Map.Strict as Map

import qualified Sara.Types as T
import qualified Sara.Syntax as S
import Sara.Meta
import Sara.Syntax
import Sara.Types
import Sara.Operators
import Sara.AstUtils
import Sara.Errors

data FunctionKey =
  FunctionKey Name [Type]
  deriving (Eq, Ord, Show)

type FunctionMap = Map.Map FunctionKey ParserSignature
type VariableMap = Map.Map Name ParserTypedVariable

checkProgram :: ParserProgram -> ErrorOr TypeCheckerProgram
checkProgram p@(Program decls _) = do
  funcs <- functions p
  decls' <- mapM (checkDeclaration funcs) decls
  return p{ program = decls' }

checkWithoutMain :: ParserProgram -> ErrorOr TypeCheckerProgram
checkWithoutMain = checkProgram

checkWithMain :: ParserProgram -> ErrorOr TypeCheckerProgram
checkWithMain p = checkMain p >> checkProgram p

checkMain :: Program a b c d -> ErrorOr ()
checkMain program = unless (hasMain program) noMain

hasMain :: Program a b c d -> Bool
hasMain = getAny . foldMapSignatures (\s -> Any $ sigName s == "main")

checkSignature :: FunctionMap -> ParserSignature -> ErrorOr TypeCheckerSignature
checkSignature funcs sig@(Signature isPure name args retType precs posts meta) = checkSignature' sig
                                                                                 >> Signature isPure name args retType
                                                                                 <$> checkConditions "precondition" precs
                                                                                 <*> checkConditions "postcondition" posts
                                                                                 <*> pure meta
  where checkConditions name conds = mapM (checkCondition name) conds
        checkCondition name cond = do
          cond' <- typeCheckFunctionBody funcs sig cond
          checkPureExpression funcs name cond'
          return cond'
        checkSignature' :: ParserSignature -> ErrorOr ()
        checkSignature' Signature{ sigName = "main", args = [], retType = T.Integer } = return ()
        checkSignature' s@Signature{ sigName = "main", args = [], retType = t }       = invalidMainRetType t (signaturePos s)
        checkSignature' s@Signature{ sigName = "main", args = a }                     = invalidMainArgs (map varType a) (signaturePos s)
        checkSignature' _                                                             = return ()

checkDeclaration :: FunctionMap -> ParserDeclaration -> ErrorOr TypeCheckerDeclaration
checkDeclaration funcs (Function sig exp meta) = do
  exp' <- typeCheckFunctionBody funcs sig exp
  checkPure funcs sig exp'
  Function <$> checkSignature funcs sig <*> pure exp' <*> pure meta
checkDeclaration funcs (Extern sig meta)       = Extern <$> checkSignature funcs sig <*> pure meta

checkPure :: FunctionMap -> ParserSignature -> TypeCheckerExpression -> ErrorOr ()
checkPure funcs sig = if isPure sig then checkExpression (checkPureExpression funcs $ sigName sig) else const $ return ()

checkPureExpression :: FunctionMap -> Name -> TypeCheckerExpression -> ErrorOr ()
checkPureExpression funcs name exp = if isPure exp then return () else impureExpression name (expressionPos exp)
  where isPure :: TypeCheckerExpression -> Bool
        isPure S.Unit{}                            = True
        isPure S.Boolean{}                         = True
        isPure S.Integer{}                         = True
        isPure S.Double{}                          = True
        isPure UnaryOperation{}                    = True
        isPure BinaryOperation{ binOp = Assign }   = False
        isPure BinaryOperation{}                   = True
        isPure Variable{}                          = True
        isPure Call{ expName = n, expArgs = args } = S.isPure sig
          where sig :: ParserSignature
                sig = fromJust $ FunctionKey n (map expressionTyp args) `Map.lookup` funcs
        isPure Conditional{}                       = True
        isPure Block{}                             = True
        isPure _                                   = False

typeCheckFunctionBody :: FunctionMap -> ParserSignature -> ParserExpression -> ErrorOr TypeCheckerExpression
typeCheckFunctionBody funcs sig body = do
  let keyedVar var@(TypedVariable varName _ _) = (varName, var)
  let vars = Map.fromList $ map keyedVar (args sig)
  body' <- typeCheckExpression funcs vars body
  let bodyType = expressionTyp body'
  let sigType = retType sig
  when (bodyType /= sigType) (invalidRetType sigType bodyType (expressionPos body'))
  return body'

functions :: ParserProgram -> ErrorOr FunctionMap
functions = functionsFromDecls . program

functionsFromDecls :: [ParserDeclaration] -> ErrorOr FunctionMap
functionsFromDecls = foldl addOneFunction (return Map.empty)

addOneFunction :: ErrorOr FunctionMap -> ParserDeclaration -> ErrorOr FunctionMap
addOneFunction funcs decl = do
  let sig = signature decl
  let pos = declarationPos decl
  funcs' <- funcs
  let f@(FunctionKey name argTypes) = functionKey sig
  case f `Map.lookup` funcs' of
    Just sig' -> redeclaredFunction name argTypes (signaturePos sig') pos
    Nothing   -> return ()
  return $ insertFunction sig funcs'

insertFunction :: ParserSignature -> FunctionMap -> FunctionMap
insertFunction sig = Map.insert (functionKey sig) sig

functionKey :: Signature a b c d -> FunctionKey
functionKey Signature{ sigName = name, args = args } = FunctionKey name $ map varType args

typeCheckExpression :: FunctionMap -> VariableMap -> ParserExpression -> ErrorOr TypeCheckerExpression
typeCheckExpression funcs vars = transformExpression (const undefined) id typeCheckSingleExpression
  where typeCheckSingleExpression exp = case exp of
          S.Boolean{}                        -> typed T.Boolean
          S.Integer{}                        -> typed T.Integer
          S.Double{}                         -> typed T.Double
          S.Unit{}                           -> typed T.Unit
          UnaryOperation op subExp _         -> typed =<< unOpType op (expressionTyp subExp)
          BinaryOperation op left right _    -> do
            when (op == Assign) (checkAssignable left) -- TODO This should be done before the type checker
            typed =<< binOpType op (expressionTyp left) (expressionTyp right)
          Variable name _ _                  -> typed =<< varType name
          Call name args _ _                 -> typed =<< funcType name (map expressionTyp args)
          Conditional cond thenExp elseExp _ -> typed =<< condType (expressionTyp cond) (expressionTyp thenExp) (expressionTyp elseExp) (expressionPos cond)
          Block _ exp _                      -> typed (expressionTyp exp)
          While cond _ _                     -> typed =<< whileType (expressionTyp cond) (expressionPos cond)
          where pos = expressionPos exp
                condType T.Boolean thenType elseType _ | thenType == elseType = return thenType
                                                       | otherwise            = mismatchingCondTypes thenType elseType pos
                condType condTyp _ _ condPos                                  = invalidCondType condTyp condPos
                whileType T.Boolean _     = return T.Unit
                whileType condTyp condPos = invalidCondType condTyp condPos
                checkAssignable :: TypeCheckerExpression -> ErrorOr ()
                checkAssignable Variable{} = return ()
                checkAssignable a          = notAssignable $ expressionPos a
                typed :: Type -> ErrorOr TypeCheckerExpression
                typed t = return exp{ expMeta = meta' }
                  where meta' :: (TypeCheckerExpressionMeta, ParserNodeMeta)
                        meta' = first (const $ TypeCheckerExpressionMeta t) (expMeta exp)
                varType :: Name -> ErrorOr Type
                varType name = case name `Map.lookup` vars of
                  Nothing -> unknownVariable name pos
                  Just t  -> return $ S.varType t
                funcType :: Name -> [Type] -> ErrorOr Type
                funcType name argTypes = case FunctionKey name argTypes `Map.lookup` funcs of
                  Nothing  -> unknownFunction name argTypes pos
                  Just sig -> return $ retType sig
                unOpType :: UnaryOperator -> Type -> ErrorOr Type
                unOpType op t = case TypedUnOp op t `Map.lookup` typedUnOps of
                  Nothing -> unknownUnOp op t pos
                  Just t  -> return t
                binOpType :: BinaryOperator -> Type -> Type -> ErrorOr Type
                binOpType op s t = case TypedBinOp op s t `Map.lookup` typedBinOps of
                  Nothing -> unknownBinOp op s t pos
                  Just t  -> return t
