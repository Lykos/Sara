module TypeChecker (
  typeCheck
  , typeCheckWithMain) where

import Text.Parsec.Pos
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Except
import Data.Functor
import Data.Either
import Data.Monoid

import qualified Data.Map.Strict as Map

import qualified Types as T
import qualified Syntax as S
import Syntax
import Types
import Operators
import AstUtils
import Errors

data FunctionType =
  FunctionType { name :: Name
               , argTypes :: [Type] }
  deriving (Eq, Ord, Show)

type FunctionMap = Map.Map FunctionType (Type, SourcePos)
type VariableMap = Map.Map Name Type

typeCheck :: Program -> ErrorOr Program
typeCheck p@(Program decls _) = do
  funcs <- functions p
  decls' <- mapM (typeCheckDeclaration funcs) decls
  return p{ program = decls' }

typeCheckWithMain :: Program -> ErrorOr Program
typeCheckWithMain p = checkMain p >> typeCheck p

checkMain :: Program -> ErrorOr ()
checkMain program = unless (hasMain program) $ noMain

hasMain :: Program -> Bool
hasMain = getAny . foldMapSignatures (\s -> Any $ S.name s == "main")

typeCheckDeclaration :: FunctionMap -> Declaration -> ErrorOr Declaration
typeCheckDeclaration funcs decl = do
  typeCheckSignature (signature decl)
  typeCheckDeclarationBody funcs decl

typeCheckSignature :: Signature -> ErrorOr ()
typeCheckSignature (Signature "main" [] T.Integer _) = return ()
typeCheckSignature (Signature "main" [] t p)         = invalidMainRetType t p
typeCheckSignature (Signature "main" a T.Integer p)  = invalidMainArgs a p
typeCheckSignature _                                 = return ()

typeCheckDeclarationBody :: FunctionMap -> Declaration -> ErrorOr Declaration
typeCheckDeclarationBody funcs (Function sig body pos) = typeCheckFunction funcs sig body pos
typeCheckDeclarationBody funcs (Method sig body pos)   = typeCheckMethod funcs sig body pos
typeCheckDeclarationBody funcs e@Extern{}              = return e

typeCheckFunction :: FunctionMap -> Signature -> Expression -> SourcePos -> ErrorOr Declaration
typeCheckFunction funcs sig exp pos = typeCheckFunctionOrMethod Function funcs sig exp pos >>= checkPureFunction
  where checkPureFunction :: Declaration -> ErrorOr Declaration
        checkPureFunction f = do
          body' <- checkPure (signature f) (body f)
          return f{ body = body' }

checkPure :: Signature -> Expression -> ErrorOr Expression
checkPure sig = transformExpression (checkPureExpression sig)
  where checkPureExpression :: Signature -> Expression -> ErrorOr Expression
        checkPureExpression sig e = if isPure e then return e else impureExpression sig (position e)
        isPure :: Expression -> Bool
        isPure S.Unit{}                          = True
        isPure S.Boolean{}                       = True
        isPure S.Integer{}                       = True
        isPure S.Double{}                        = True
        isPure UnaryOperation{}                  = True
        isPure BinaryOperation{ binOp = Assign } = False
        isPure BinaryOperation{}                 = True
        isPure Variable{}                        = True
        isPure Call{}                            = True
        isPure Conditional{}                     = True
        isPure Block{}                           = True
        isPure _                                 = False

typeCheckMethod :: FunctionMap -> Signature -> Expression -> SourcePos -> ErrorOr Declaration
typeCheckMethod = typeCheckFunctionOrMethod Method

typeCheckFunctionOrMethod :: FunctionOrMethodConstructor -> FunctionMap -> Signature -> Expression -> SourcePos -> ErrorOr Declaration
typeCheckFunctionOrMethod constructor funcs sig body pos =
  let var (TypedVariable varName varType _) = (varName, varType)
      vars = Map.fromList $ map var (args sig)
  in liftM2 (constructor sig) (typeCheckExpression funcs vars body) (return pos)

functions :: Program -> ErrorOr FunctionMap
functions = functionsFromDecls . program

functionsFromDecls :: [Declaration] -> ErrorOr FunctionMap
functionsFromDecls = foldr addOneFunction (return Map.empty)

addOneFunction :: Declaration -> ErrorOr FunctionMap -> ErrorOr FunctionMap
addOneFunction decl map = do
  let sig = signature decl
  let pos = position decl
  map' <- map
  case functionType sig `Map.lookup` map' of
    Just (_, originalPos) -> redeclaredFunction sig originalPos pos
    Nothing               -> return ()
  return $ insertFunction sig map'

insertFunction :: Signature -> FunctionMap -> FunctionMap
insertFunction f = Map.insert (functionType f) (typ f, position f)

functionType :: Signature -> FunctionType
functionType (Signature name args _ _) = FunctionType name $ map varType args

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
                                        | otherwise            -> mismatchingCondTypes (typ typedThenExp) (typ typedElseExp) pos
        (condType, _, _)                                       -> invalidCondType (typ cond) pos
    Block stmts exp _ _ -> do
      typedStmts <- mapM typedSubExp stmts
      typedExp <- typedSubExp exp
      return $ Block typedStmts typedExp (typ typedExp) pos
    While cond body _ _ -> do
      typedCond <- typedSubExp cond
      typedBody <- typedSubExp body
      let condType = typ typedCond
      when (condType /= T.Boolean) (invalidCondType (typ cond) pos)
      return $ While typedCond typedBody T.Unit pos
  where pos = position checkedExp
        typedSubExp = typeCheckExpression funcs vars
        checkAssignable :: Expression -> ErrorOr ()
        checkAssignable (Variable _ _ _) = return ()
        checkAssignable a                = notAssignable pos
        typed :: Type -> ErrorOr Expression
        typed t = return checkedExp{ expType = t }
        varType :: Name -> ErrorOr Type
        varType name = case name `Map.lookup` vars of
          Nothing -> unknownVariable name pos
          Just t  -> return t
        funcType :: Name -> [Type] -> ErrorOr Type
        funcType name argTypes = case FunctionType name argTypes `Map.lookup` funcs of
          Nothing     -> unknownFunction name argTypes pos
          Just (t, _) -> return t

unOpType :: UnaryOperator -> Type -> SourcePos -> ErrorOr Type
unOpType op t pos = case TypedUnOp op t `Map.lookup` typedUnOps of
  Nothing -> unknownUnOp op t pos
  Just t  -> return t

binOpType :: BinaryOperator -> Type -> Type -> SourcePos -> ErrorOr Type
binOpType op s t pos = case TypedBinOp op s t `Map.lookup` typedBinOps of
  Nothing -> unknownBinOp op s t pos
  Just t  -> return t
