module Sara.TypeChecker (
  checkWithoutMain
  , checkWithMain) where

import Text.Parsec.Pos
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Except
import Data.Functor
import Data.Either
import Data.Monoid
import Data.Maybe

import qualified Data.Map.Strict as Map

import qualified Sara.Types as T
import qualified Sara.Syntax as S
import Sara.Syntax
import Sara.Types
import Sara.Operators
import Sara.AstUtils
import Sara.Errors

data FunctionKey =
  FunctionKey { name :: Name
               , argTypes :: [Type] }
  deriving (Eq, Ord, Show)

type FunctionMap = Map.Map FunctionKey Signature
type VariableMap = Map.Map Name TypedVariable

checkProgram :: Program -> ErrorOr Program
checkProgram p@(Program decls _) = do
  funcs <- functions p
  decls' <- mapM (checkDeclaration funcs) decls
  return p{ program = decls' }

checkWithoutMain = checkProgram

checkWithMain :: Program -> ErrorOr Program
checkWithMain p = checkMain p >> checkProgram p

checkMain :: Program -> ErrorOr ()
checkMain program = unless (hasMain program) noMain

hasMain :: Program -> Bool
hasMain = getAny . foldMapSignatures (\s -> Any $ S.name s == "main")

checkDeclaration :: FunctionMap -> Declaration -> ErrorOr Declaration
checkDeclaration funcs decl = do
  checkSignature (signature decl)
  checkDeclarationBody funcs decl

checkSignature :: Signature -> ErrorOr ()
checkSignature (Signature _ "main" [] T.Integer _) = return ()
checkSignature (Signature _ "main" [] t p)         = invalidMainRetType t p
checkSignature (Signature _ "main" a T.Integer p)  = invalidMainArgs (map typ a) p
checkSignature _                                   = return ()

checkDeclarationBody :: FunctionMap -> Declaration -> ErrorOr Declaration
checkDeclarationBody funcs f@(Function sig exp pos) = do
  exp' <- typeCheckFunctionBody funcs sig exp pos
  exp'' <- checkPure funcs sig exp'
  return f{ body = exp'' }
checkDeclarationBody funcs e@Extern{}               = return e

checkPure :: FunctionMap -> Signature -> Expression -> ErrorOr Expression
checkPure funcs sig = if S.pure sig then transformExpression checkPureExpression else return
  where checkPureExpression :: Expression -> ErrorOr Expression
        checkPureExpression e = if isPure e then return e else impureExpression (S.name sig) (position e)
        isPure :: Expression -> Bool
        isPure S.Unit{}                            = True
        isPure S.Boolean{}                         = True
        isPure S.Integer{}                         = True
        isPure S.Double{}                          = True
        isPure UnaryOperation{}                    = True
        isPure BinaryOperation{ binOp = Assign }   = False
        isPure BinaryOperation{}                   = True
        isPure Variable{}                          = True
        isPure Call{ expName = n, expArgs = args } = S.pure . fromJust $ FunctionKey n (map typ args) `Map.lookup` funcs
        isPure Conditional{}                       = True
        isPure Block{}                             = True
        isPure _                                   = False

typeCheckFunctionBody :: FunctionMap -> Signature -> Expression -> SourcePos -> ErrorOr Expression
typeCheckFunctionBody funcs sig body pos = do
  let keyedVar var@(TypedVariable varName _ _) = (varName, var)
  let vars = Map.fromList $ map keyedVar (args sig)
  body' <- typeCheckExpression funcs vars body
  let bodyType = typ body'
  let sigType = typ sig
  when (bodyType /= sigType) (invalidRetType sigType bodyType (position body'))
  return body'

functions :: Program -> ErrorOr FunctionMap
functions = functionsFromDecls . program

functionsFromDecls :: [Declaration] -> ErrorOr FunctionMap
functionsFromDecls = foldl addOneFunction (return Map.empty)

addOneFunction :: ErrorOr FunctionMap -> Declaration -> ErrorOr FunctionMap
addOneFunction funcs decl = do
  let sig = signature decl
  let pos = position decl
  funcs' <- funcs
  let f@(FunctionKey name argTypes) = functionKey sig
  case f `Map.lookup` funcs' of
    Just sig' -> redeclaredFunction name argTypes (position sig') pos
    Nothing   -> return ()
  return $ insertFunction sig funcs'

insertFunction :: Signature -> FunctionMap -> FunctionMap
insertFunction sig = Map.insert (functionKey sig) sig

functionKey :: Signature -> FunctionKey
functionKey (Signature _ name args _ _)   = FunctionKey name $ map varType args

typeCheckExpression :: FunctionMap -> VariableMap -> Expression -> ErrorOr Expression
typeCheckExpression funcs vars checkedExp = case checkedExp of
    S.Unit{} -> typed T.Unit
    S.Boolean{} -> typed T.Boolean
    S.Integer{} -> typed T.Integer
    S.Double{} -> typed T.Double
    UnaryOperation op subExp _ _ -> do
      typedSubExp <- typedSubExp subExp
      let subExpType = typ typedSubExp
      t <- unOpType op subExpType pos
      return $ UnaryOperation op typedSubExp t pos
    BinaryOperation op left right _ _ -> do
      typedLeft <- typedSubExp left
      typedRight <- typedSubExp right
      let leftType = typ typedLeft
      let rightType = typ typedRight
      when (op == Assign) (checkAssignable typedLeft)
      t <- binOpType op leftType rightType pos
      return $ BinaryOperation op typedLeft typedRight t pos
    Variable name _ _ -> varType name >>= typed
    Call name args _ _-> do
      typedArgs <- mapM typedSubExp args
      let argTypes = map typ typedArgs
      t <- funcType name argTypes
      return $ Call name typedArgs t pos
    Conditional cond thenExp elseExp _ _ -> do
      typedCond <- typedSubExp cond
      typedThenExp <- typedSubExp thenExp
      typedElseExp <- typedSubExp elseExp
      let condType = typ typedCond
      let thenType = typ typedThenExp
      let elseType = typ typedElseExp
      case (condType, thenType, elseType) of
        (T.Boolean, thenType, elseType) | thenType == elseType -> return $ Conditional typedCond typedThenExp typedElseExp thenType pos
                                        | otherwise            -> mismatchingCondTypes thenType elseType pos
        (condType, _, _)                                       -> invalidCondType condType (position cond)
    Block stmts exp _ _ -> do
      typedStmts <- mapM typedSubExp stmts
      typedExp <- typedSubExp exp
      return $ Block typedStmts typedExp (typ typedExp) pos
    While cond body _ _ -> do
      typedCond <- typedSubExp cond
      typedBody <- typedSubExp body
      let condType = typ typedCond
      when (condType /= T.Boolean) (invalidCondType condType (position cond))
      return $ While typedCond typedBody T.Unit pos
  where pos = position checkedExp
        typedSubExp = typeCheckExpression funcs vars
        checkAssignable :: Expression -> ErrorOr ()
        checkAssignable Variable{} = return ()
        checkAssignable a          = notAssignable (position a)
        typed :: Type -> ErrorOr Expression
        typed t = return checkedExp{ expType = t }
        varType :: Name -> ErrorOr Type
        varType name = case name `Map.lookup` vars of
          Nothing -> unknownVariable name pos
          Just t  -> return $ typ t
        funcType :: Name -> [Type] -> ErrorOr Type
        funcType name argTypes = case FunctionKey name argTypes `Map.lookup` funcs of
          Nothing  -> unknownFunction name argTypes pos
          Just sig -> return $ typ sig

unOpType :: UnaryOperator -> Type -> SourcePos -> ErrorOr Type
unOpType op t pos = case TypedUnOp op t `Map.lookup` typedUnOps of
  Nothing -> unknownUnOp op t pos
  Just t  -> return t

binOpType :: BinaryOperator -> Type -> Type -> SourcePos -> ErrorOr Type
binOpType op s t pos = case TypedBinOp op s t `Map.lookup` typedBinOps of
  Nothing -> unknownBinOp op s t pos
  Just t  -> return t
