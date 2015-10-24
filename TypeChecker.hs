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

type FunctionMap = Map.Map FunctionType Type
type VariableMap = Map.Map Name Type

typeCheck :: Program -> ErrorOr Program
typeCheck p@(Program decls _) = do
  funcs <- functions p
  decls' <- mapM (typeCheckDeclaration funcs) decls
  return p{ program = decls' }

typeCheckWithMain :: Program -> ErrorOr Program
typeCheckWithMain p = checkMain p >> typeCheck p

checkMain :: Program -> ErrorOr ()
checkMain program = unless (hasMain program) $ noMain (position program)

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
          body' <- checkPure $ body f
          return f{ body = body' }

checkPure :: Expression -> ErrorOr Expression
checkPure = transformExpression checkPureExpression
  where checkPureExpression :: Expression -> ErrorOr Expression
        checkPureExpression e = if isPure e then return e else impureExpression e
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
  when (functionType sig `Map.member` map') (ambiguousFunction sig pos)
  return $ insertFunction sig map'

insertFunction :: Signature -> FunctionMap -> FunctionMap
insertFunction f = Map.insert (functionType f) (typ f)

functionType :: Signature -> FunctionType
functionType (Signature name args _ _) = FunctionType name $ map varType args

typeCheckExpression :: FunctionMap -> VariableMap -> Expression -> ErrorOr Expression
typeCheckExpression funcs vars e = case e of
    S.Unit{} -> typed T.Unit
    S.Boolean{} -> typed T.Boolean
    S.Integer{} -> typed T.Integer
    S.Double{} -> typed T.Double
    UnaryOperation op subExp _ _ -> do
      typedSubExp <- typedSubExp subExp
      subExpType <- astType typedSubExp
      addType (UnaryOperation op typedSubExp) (unOpType op subExpType pos)
    BinaryOperation op left right _ _ -> do
      typedLeft <- typedSubExp left
      typedRight <- typedSubExp right
      leftType <- astType typedLeft
      rightType <- astType typedRight
      when (op == Assign) (checkAssignable typedLeft)
      addType (BinaryOperation op typedLeft typedRight) (binOpType op leftType rightType pos)
    Variable name _ _ -> addType (Variable name) (varType name)
    Call name args _ _-> do
      typedArgs <- mapM typedSubExp args
      argTypes <- mapM astType typedArgs
      addType (Call name typedArgs) (funcType name argTypes)
    Conditional cond ifExp elseExp _ _ -> do
      typedCond <- typedSubExp cond
      typedIfExp <- typedSubExp ifExp
      typedElseExp <- typedSubExp elseExp
      condType <- astType typedCond
      ifType <- astType typedIfExp
      elseType <- astType typedElseExp
      case (condType, ifType, elseType) of
        (T.Boolean, ifType, elseType) | ifType == elseType    -> addType (Conditional typedCond typedIfExp typedElseExp) (return ifType)
                                      | otherwise             -> mismatchingCondTypes typedIfExp typedElseExp pos
        (condType, _, _)                                          -> invalidCondType cond
    Block stmts exp _ _ -> do
      typedStmts <- mapM typedSubExp stmts
      typedExp <- typedSubExp exp
      addType (Block typedStmts typedExp) (astType typedExp)
    While cond body _ _ -> do
      typedCond <- typedSubExp cond
      typedBody <- typedSubExp body
      condType <- astType typedCond
      when (condType /= T.Boolean) (invalidCondType cond)
      addType (While typedCond typedBody) (return T.Unit)
  where pos = position e
        typedSubExp = typeCheckExpression funcs vars
        checkAssignable :: Expression -> ErrorOr ()
        checkAssignable (Variable _ _ _) = return ()
        checkAssignable a                = notAssignable a
        checkNotUnknown :: Type -> SourcePos -> ErrorOr Type
        checkNotUnknown Unknown pos = unknownType pos
        checkNotUnknown t _         = return t
        astType :: Expression -> ErrorOr Type
        astType exp = checkNotUnknown (expType exp) (expPos exp)
        typed :: Type -> ErrorOr Expression
        typed t = do
          s <- checkNotUnknown t pos
          return e { expType = s }
        addType :: (Type -> SourcePos -> Expression) -> ErrorOr Type -> ErrorOr Expression
        addType exp t = do
          t' <- t
          return $ exp t' pos
        varType :: Name -> ErrorOr Type
        varType name = case name `Map.lookup` vars of
          Nothing -> unknownVariable name pos
          Just t  -> return t
        funcType :: Name -> [Type] -> ErrorOr Type
        funcType name argTypes = case FunctionType name argTypes `Map.lookup` funcs of
          Nothing -> unknownFunction name argTypes pos
          Just t  -> return t

unOpType :: UnaryOperator -> Type -> SourcePos -> ErrorOr Type
unOpType op t pos = case TypedUnOp op t `Map.lookup` typedUnOps of
  Nothing -> unknownUnOp op t pos
  Just t  -> return t

binOpType :: BinaryOperator -> Type -> Type -> SourcePos -> ErrorOr Type
binOpType op s t pos = case TypedBinOp op s t `Map.lookup` typedBinOps of
  Nothing -> unknownBinOp op s t pos
  Just t  -> return t

unknownUnOp :: UnaryOperator -> Type -> SourcePos -> ErrorOr a
unknownUnOp op t =
  typeError $ "Unknown operator " ++ show op ++ " for expression of type " ++ show t ++ "."

unknownBinOp :: BinaryOperator -> Type -> Type -> SourcePos -> ErrorOr a
unknownBinOp op s t =
  typeError $ "Unknown operator " ++ show op ++ " for expressions of type " ++ show s ++ " and " ++ show t ++ "."

unknownVariable :: Name -> SourcePos -> ErrorOr a
unknownVariable name =
  typeError $ "Unknown variable " ++ name ++ "."

unknownFunction :: Name -> [Type] -> SourcePos -> ErrorOr a
unknownFunction name argTypes =
  typeError $ "Unknown function " ++ name ++ " for argument types " ++ show argTypes ++ "."

invalidCondType :: Expression -> ErrorOr a
invalidCondType cond =
  typeError ("Conditions of conditionals have to have type " ++ show T.Boolean ++ ", found " ++ show cond ++ ".") (expPos cond)

mismatchingCondTypes :: Expression -> Expression -> SourcePos -> ErrorOr a
mismatchingCondTypes ifExp elseExp =
  typeError $ "If and Else branch of conditionals have to have the same types, found " ++ show ifExp ++ " and " ++ show elseExp ++ "."

ambiguousFunction :: Signature -> SourcePos -> ErrorOr a
ambiguousFunction sig =
  typeError $ "Function " ++ show sig ++ " is ambiguous. A function with the same name and the same argument types already exists."

unknownType :: SourcePos -> ErrorOr a
unknownType = typeError "Unknown type."

invalidMainArgs :: [TypedVariable] -> SourcePos -> ErrorOr a
invalidMainArgs args =
  typeError $ "Main function should not have arguments. Found " ++ show args ++ " instead."

invalidMainRetType :: Type -> SourcePos -> ErrorOr a
invalidMainRetType t =
  typeError $ "Main function should have return type Integer. Found " ++ show t ++ " instead."

noMain :: SourcePos -> ErrorOr a
noMain = typeError "The program has no main funtion."

impureExpression :: Expression -> ErrorOr a
impureExpression e =
  typeError ("Functions can only contain pure expressions, but expression " ++ show e ++ " is not pure.") (S.position e)

notAssignable :: Expression -> ErrorOr a
notAssignable a =
  typeError ("Not a valid assignment target " ++ show a ++ ".") (S.position a)
