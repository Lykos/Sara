module TypeChecker (
  TypeErrorOr(..)
  , TypeError(..)
  , typeCheck
  , typeCheckWithMain) where

import Text.Parsec.Pos
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Either
import Types
import Syntax
import Operators
import AstUtils
import Data.Monoid
import qualified Data.Map.Strict as Map

data FunctionType =
  FunctionType { name :: Name
               , argTypes :: [Type] }
  deriving (Eq, Ord, Show)

type FunctionMap = Map.Map FunctionType Type
type VariableMap = Map.Map Name Type
data TypeErrorOr a
  = Error TypeError
  | Result a
  deriving (Eq, Ord, Show)

data TypeError =
  TypeError { errorPos :: SourcePos
            , errorMsg :: String }
  deriving (Eq, Ord)

instance Show TypeError where
  show err = show (errorPos err) ++ ":\n" ++ errorMsg err

instance Monad TypeErrorOr where
  Error e >>= f  = Error e
  Result r >>= f = f r
  return         = Result

instance Applicative TypeErrorOr where
  pure  = return
  (<*>) = ap

instance Functor TypeErrorOr where
  fmap f xs = xs >>= return . f

typeCheck :: Program -> TypeErrorOr Program
typeCheck (Program prog) = do
  funcs <- functions $ Program prog
  liftM Program $ sequence $ map (typeCheckDeclarationAst funcs) prog

typeCheckWithMain :: Program -> TypeErrorOr Program
typeCheckWithMain p = checkMain p >> typeCheck p

checkMain :: Program -> TypeErrorOr ()
checkMain program = if hasMain program then Result () else noMain (programPos program)

programPos :: Program -> SourcePos
programPos (Program ((DeclarationAst _ pos):xs)) = pos
programPos _                                     = newPos "<unknown>" 0 0

hasMain :: Program -> Bool
hasMain = getAny . foldMapSignatures (\s -> Any $ funcName s == "main")

typeCheckDeclarationAst :: FunctionMap -> DeclarationAst -> TypeErrorOr DeclarationAst
typeCheckDeclarationAst funcs declAst@(DeclarationAst decl pos) = do
  typeCheckSignature (signature decl) pos
  typedDecl <- typeCheckDeclaration funcs $ decl
  return declAst { decl = typedDecl }

typeCheckSignature :: Signature -> SourcePos -> TypeErrorOr ()
typeCheckSignature (Signature "main" [] Types.Integer) _ = Result ()
typeCheckSignature (Signature "main" [] t) p             = invalidMainRetType t p
typeCheckSignature (Signature "main" a Types.Integer) p  = invalidMainArgs a p
typeCheckSignature _ _                                   = Result ()

typeCheckDeclaration :: FunctionMap -> Declaration -> TypeErrorOr Declaration
typeCheckDeclaration funcs (Function sig body) = typeCheckFunction funcs sig body
typeCheckDeclaration funcs (Method sig body)   = typeCheckMethod funcs sig body
typeCheckDeclaration funcs e@(Extern _)        = return e

typeCheckFunction :: FunctionMap -> Signature -> ExpressionAst -> TypeErrorOr Declaration
typeCheckFunction funcs sig body = typeCheckFunctionOrMethod Function funcs sig body >>= checkPureFunction
  where checkPureFunction :: Declaration -> TypeErrorOr Declaration
        checkPureFunction (Function sig body) = do
          body' <- checkPure body
          return $ Function sig body

checkPure :: ExpressionAst -> TypeErrorOr ExpressionAst
checkPure e = transformExpressionAst checkPureExpressionAst e
  where checkPureExpressionAst :: ExpressionAst -> TypeErrorOr ExpressionAst
        checkPureExpressionAst ea@(ExpressionAst e _ _) | isPure e  = return ea
                                                        | otherwise = impureExpression ea
        isPure :: Expression -> Bool
        isPure Syntax.Unit             = True
        isPure (Syntax.Boolean _)      = True
        isPure (Syntax.Integer _)      = True
        isPure (Syntax.Double _)       = True
        isPure (UnaryOperation _ _)    = True
        isPure (BinaryOperation _ _ _) = True
        isPure (Variable _)            = True
        isPure (Call _ _)              = True
        isPure (Conditional _ _ _)     = True
        isPure (Block _ _)             = True
        isPure _                       = False

typeCheckMethod :: FunctionMap -> Signature -> ExpressionAst -> TypeErrorOr Declaration
typeCheckMethod = typeCheckFunctionOrMethod Method

typeCheckFunctionOrMethod :: FunctionOrMethodConstructor -> FunctionMap -> Signature -> ExpressionAst -> TypeErrorOr Declaration
typeCheckFunctionOrMethod constructor funcs sig body =
  let var (TypedVariable varName varType _) = (varName, varType)
      vars = Map.fromList $ map var (args sig)
  in typeCheckExp funcs vars body >>= return . constructor sig

functions :: Program -> TypeErrorOr FunctionMap
functions = functionsFromDecls . program

functionsFromDecls :: [DeclarationAst] -> TypeErrorOr FunctionMap
functionsFromDecls d = foldr addOneFunction (Result Map.empty) d

addOneFunction :: DeclarationAst -> TypeErrorOr FunctionMap -> TypeErrorOr FunctionMap
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

typeCheckExp :: FunctionMap -> VariableMap -> ExpressionAst -> TypeErrorOr ExpressionAst
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
        (Types.Boolean, ifType, elseType) | ifType == elseType    -> addType (Conditional typedCond typedIfExp typedElseExp) (Result ifType)
                                          | otherwise             -> mismatchingCondTypes typedIfExp typedElseExp pos
        (condType, _, _)                                          -> invalidCondType cond
    Block stmts exp -> do
      typedStmts <- sequence $ map typedSubExp stmts
      typedExp <- typedSubExp exp
      addType (Block typedStmts typedExp) (astType typedExp)
  where pos = expPos ast
        typedSubExp = typeCheckExp funcs vars
        checkNotUnknown :: Type -> SourcePos -> TypeErrorOr Type
        checkNotUnknown Unknown pos = unknownType pos
        checkNotUnknown t _         = Result t
        astType :: ExpressionAst -> TypeErrorOr Type
        astType exp = checkNotUnknown (expType exp) (expPos exp)
        typedAst :: Type -> TypeErrorOr ExpressionAst
        typedAst t = do
          s <- checkNotUnknown t pos
          return ast { expType = s }
        addType :: Expression -> TypeErrorOr Type -> TypeErrorOr ExpressionAst
        addType e t = do
          s <- t
          return ast { astExp = e , expType = s }
        varType :: Name -> TypeErrorOr Type
        varType name = case name `Map.lookup` vars of
          Nothing -> unknownVariable name pos
          Just t  -> Result t
        funcType :: Name -> [Type] -> TypeErrorOr Type
        funcType name argTypes = case (FunctionType name argTypes) `Map.lookup` funcs of
          Nothing -> unknownFunction name argTypes pos
          Just t  -> Result t

unOpType :: UnaryOperator -> Type -> SourcePos -> TypeErrorOr Type
unOpType op t pos = case (TypedUnOp op t) `Map.lookup` typedUnOps of
  Nothing -> unknownUnOp op t pos
  Just t  -> Result t

binOpType :: BinaryOperator -> Type -> Type -> SourcePos -> TypeErrorOr Type
binOpType op s t pos = case (TypedBinOp op s t) `Map.lookup` typedBinOps of
  Nothing -> unknownBinOp op s t pos
  Just t  -> Result t

unknownUnOp :: UnaryOperator -> Type -> SourcePos -> TypeErrorOr a
unknownUnOp op t =
  typeError $ "Unknown operator " ++ show op ++ " for expression of type " ++ show t ++ "."

unknownBinOp :: BinaryOperator -> Type -> Type -> SourcePos -> TypeErrorOr a
unknownBinOp op s t =
  typeError $ "Unknown operator " ++ show op ++ " for expressions of type " ++ show s ++ " and " ++ show t ++ "."

unknownVariable :: Name -> SourcePos -> TypeErrorOr a
unknownVariable name =
  typeError $ "Unknown variable " ++ name ++ "."

unknownFunction :: Name -> [Type] -> SourcePos -> TypeErrorOr a
unknownFunction name argTypes =
  typeError $ "Unknown function " ++ name ++ " for argument types " ++ show argTypes ++ "."

invalidCondType :: ExpressionAst -> TypeErrorOr a
invalidCondType cond =
  typeError ("Conditions of conditionals have to have type " ++ show Types.Boolean ++ ", found " ++ show cond ++ ".") (expPos cond)

mismatchingCondTypes :: ExpressionAst -> ExpressionAst -> SourcePos -> TypeErrorOr a
mismatchingCondTypes ifExp elseExp =
  typeError $ "If and Else branch of conditionals have to have the same types, found " ++ show ifExp ++ " and " ++ show elseExp ++ "."

ambiguousFunction :: Signature -> SourcePos -> TypeErrorOr a
ambiguousFunction sig =
  typeError $ "Function " ++ show sig ++ " is ambiguous. A function with the same name and the same argument types already exists."

unknownType :: SourcePos -> TypeErrorOr a
unknownType = typeError "Unknown type."

invalidMainArgs :: [TypedVariable] -> SourcePos -> TypeErrorOr a
invalidMainArgs args =
  typeError $ "Main function should not have arguments. Found " ++ show args ++ " instead."

invalidMainRetType :: Type -> SourcePos -> TypeErrorOr a
invalidMainRetType t =
  typeError $ "Main function should have return type Integer. Found " ++ show t ++ " instead."

noMain :: SourcePos -> TypeErrorOr a
noMain = typeError "The program has no main funtion."

impureExpression :: ExpressionAst -> TypeErrorOr a
impureExpression e =
  typeError ("Functions can only contain pure expressions, but expression " ++ show e ++ " is not pure.") (expPos e)

typeError :: String -> SourcePos -> TypeErrorOr a
typeError msg pos = Error $ TypeError pos msg
