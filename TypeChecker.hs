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

import Types
import Syntax
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
typeCheck (Program prog) = do
  funcs <- functions $ Program prog
  liftM Program $ sequence $ map (typeCheckDeclarationAst funcs) prog

typeCheckWithMain :: Program -> ErrorOr Program
typeCheckWithMain p = checkMain p >> typeCheck p

checkMain :: Program -> ErrorOr ()
checkMain program = if hasMain program then return () else noMain (programPos program)

programPos :: Program -> SourcePos
programPos (Program ((DeclarationAst _ pos):xs)) = pos
programPos _                                     = newPos "<unknown>" 0 0

hasMain :: Program -> Bool
hasMain = getAny . foldMapSignatures (\s -> Any $ funcName s == "main")

typeCheckDeclarationAst :: FunctionMap -> DeclarationAst -> ErrorOr DeclarationAst
typeCheckDeclarationAst funcs declAst@(DeclarationAst decl pos) = do
  typeCheckSignature (signature decl) pos
  typedDecl <- typeCheckDeclaration funcs $ decl
  return declAst { decl = typedDecl }

typeCheckSignature :: Signature -> SourcePos -> ErrorOr ()
typeCheckSignature (Signature "main" [] Types.Integer) _ = return ()
typeCheckSignature (Signature "main" [] t) p             = invalidMainRetType t p
typeCheckSignature (Signature "main" a Types.Integer) p  = invalidMainArgs a p
typeCheckSignature _ _                                   = return ()

typeCheckDeclaration :: FunctionMap -> Declaration -> ErrorOr Declaration
typeCheckDeclaration funcs (Function sig body) = typeCheckFunction funcs sig body
typeCheckDeclaration funcs (Method sig body)   = typeCheckMethod funcs sig body
typeCheckDeclaration funcs e@(Extern _)        = return e

typeCheckFunction :: FunctionMap -> Signature -> ExpressionAst -> ErrorOr Declaration
typeCheckFunction funcs sig body = typeCheckFunctionOrMethod Function funcs sig body >>= checkPureFunction
  where checkPureFunction :: Declaration -> ErrorOr Declaration
        checkPureFunction (Function sig body) = do
          body' <- checkPure body
          return $ Function sig body

checkPure :: ExpressionAst -> ErrorOr ExpressionAst
checkPure e = transformExpressionAst checkPureExpressionAst e
  where checkPureExpressionAst :: ExpressionAst -> ErrorOr ExpressionAst
        checkPureExpressionAst ea@(ExpressionAst e _ _) | isPure e  = return ea
                                                        | otherwise = impureExpression ea
        isPure :: Expression -> Bool
        isPure Syntax.Unit                  = True
        isPure (Syntax.Boolean _)           = True
        isPure (Syntax.Integer _)           = True
        isPure (Syntax.Double _)            = True
        isPure (UnaryOperation _ _)         = True
        isPure (BinaryOperation Assign _ _) = False
        isPure (BinaryOperation _ _ _)      = True
        isPure (Variable _)                 = True
        isPure (Call _ _)                   = True
        isPure (Conditional _ _ _)          = True
        isPure (Block _ _)                  = True
        isPure _                            = False

typeCheckMethod :: FunctionMap -> Signature -> ExpressionAst -> ErrorOr Declaration
typeCheckMethod = typeCheckFunctionOrMethod Method

typeCheckFunctionOrMethod :: FunctionOrMethodConstructor -> FunctionMap -> Signature -> ExpressionAst -> ErrorOr Declaration
typeCheckFunctionOrMethod constructor funcs sig body =
  let var (TypedVariable varName varType _) = (varName, varType)
      vars = Map.fromList $ map var (args sig)
  in typeCheckExp funcs vars body >>= return . constructor sig

functions :: Program -> ErrorOr FunctionMap
functions = functionsFromDecls . program

functionsFromDecls :: [DeclarationAst] -> ErrorOr FunctionMap
functionsFromDecls d = foldr addOneFunction (return Map.empty) d

addOneFunction :: DeclarationAst -> ErrorOr FunctionMap -> ErrorOr FunctionMap
addOneFunction d m = do
  let sig = signature . decl $ d
  let pos = declPos d
  n <- m
  when ((functionType sig) `Map.member` n) (ambiguousFunction sig pos)
  return $ insertFunction sig n

insertFunction :: Signature -> FunctionMap -> FunctionMap
insertFunction f = Map.insert (functionType f) (returnType f)

functionType :: Signature -> FunctionType
functionType (Signature name args _) = FunctionType name $ map varType args

typeCheckExp :: FunctionMap -> VariableMap -> ExpressionAst -> ErrorOr ExpressionAst
typeCheckExp funcs vars ast =
  let e = astExp ast
  in case e :: Expression of
    Syntax.Unit -> typedAst Types.Unit
    Syntax.Boolean _ -> typedAst Types.Boolean
    Syntax.Integer _ -> typedAst Types.Integer
    Syntax.Double _ -> typedAst Types.Double
    UnaryOperation op subExp -> do
      typedSubExp <- typedSubExp subExp
      subExpType <- astType typedSubExp
      addType (UnaryOperation op typedSubExp) (unOpType op subExpType pos)
    BinaryOperation op left right -> do
      typedLeft <- typedSubExp left
      typedRight <- typedSubExp right
      leftType <- astType typedLeft
      rightType <- astType typedRight
      when (op == Assign) (checkAssignable typedLeft)
      addType (BinaryOperation op typedLeft typedRight) (binOpType op leftType rightType pos)
    Variable name -> addType e (varType name)
    Call name args -> do
      typedArgs <- sequence $ map typedSubExp args
      argTypes <- sequence $ map astType typedArgs
      addType (Call name typedArgs) (funcType name argTypes)
    Conditional cond ifExp elseExp -> do
      typedCond <- typedSubExp cond
      typedIfExp <- typedSubExp ifExp
      typedElseExp <- typedSubExp elseExp
      condType <- astType typedCond
      ifType <- astType typedIfExp
      elseType <- astType typedElseExp
      case (condType, ifType, elseType) of
        (Types.Boolean, ifType, elseType) | ifType == elseType    -> addType (Conditional typedCond typedIfExp typedElseExp) (return ifType)
                                          | otherwise             -> mismatchingCondTypes typedIfExp typedElseExp pos
        (condType, _, _)                                          -> invalidCondType cond
    Block stmts exp -> do
      typedStmts <- sequence $ map typedSubExp stmts
      typedExp <- typedSubExp exp
      addType (Block typedStmts typedExp) (astType typedExp)
    While cond body -> do
      typedCond <- typedSubExp cond
      typedBody <- typedSubExp body
      condType <- astType typedCond
      when (condType /= Types.Boolean) (invalidCondType cond)
      addType (While typedCond typedBody) (return Types.Unit)
  where pos = expPos ast
        typedSubExp = typeCheckExp funcs vars
        checkAssignable :: ExpressionAst -> ErrorOr ()
        checkAssignable (ExpressionAst (Variable _) _ _) = return ()
        checkAssignable a                                = notAssignable a
        checkNotUnknown :: Type -> SourcePos -> ErrorOr Type
        checkNotUnknown Unknown pos = unknownType pos
        checkNotUnknown t _         = return t
        astType :: ExpressionAst -> ErrorOr Type
        astType exp = checkNotUnknown (expType exp) (expPos exp)
        typedAst :: Type -> ErrorOr ExpressionAst
        typedAst t = do
          s <- checkNotUnknown t pos
          return ast { expType = s }
        addType :: Expression -> ErrorOr Type -> ErrorOr ExpressionAst
        addType e t = do
          s <- t
          return ast { astExp = e , expType = s }
        varType :: Name -> ErrorOr Type
        varType name = case name `Map.lookup` vars of
          Nothing -> unknownVariable name pos
          Just t  -> return t
        funcType :: Name -> [Type] -> ErrorOr Type
        funcType name argTypes = case (FunctionType name argTypes) `Map.lookup` funcs of
          Nothing -> unknownFunction name argTypes pos
          Just t  -> return t

unOpType :: UnaryOperator -> Type -> SourcePos -> ErrorOr Type
unOpType op t pos = case (TypedUnOp op t) `Map.lookup` typedUnOps of
  Nothing -> unknownUnOp op t pos
  Just t  -> return t

binOpType :: BinaryOperator -> Type -> Type -> SourcePos -> ErrorOr Type
binOpType op s t pos = case (TypedBinOp op s t) `Map.lookup` typedBinOps of
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

invalidCondType :: ExpressionAst -> ErrorOr a
invalidCondType cond =
  typeError ("Conditions of conditionals have to have type " ++ show Types.Boolean ++ ", found " ++ show cond ++ ".") (expPos cond)

mismatchingCondTypes :: ExpressionAst -> ExpressionAst -> SourcePos -> ErrorOr a
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

impureExpression :: ExpressionAst -> ErrorOr a
impureExpression e =
  typeError ("Functions can only contain pure expressions, but expression " ++ show e ++ " is not pure.") (expPos e)

notAssignable :: ExpressionAst -> ErrorOr a
notAssignable a =
  typeError ("Not a valid assignment target " ++ show a ++ ".") (expPos a)
