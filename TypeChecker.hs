module TypeChecker (
  TypeErrorOr(..)
  , TypeError
  , typeCheck) where

import Text.Parsec.Pos
import Control.Monad
import Data.Either
import Types
import Syntax
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

data TypeError =
  TypeError { errorPos :: SourcePos
            , errorMsg :: String }
  deriving (Eq, Ord)
instance Show TypeError where
  show err = show (errorPos err) ++ ":\n" ++ errorMsg err

instance Functor TypeErrorOr where
  fmap f (Error e)  = Error e
  fmap f (Result a) = Result $ f a

instance Monad TypeErrorOr where
  Error e >>= f  = Error e
  Result r >>= f = f r
  return         = Result

typeCheck :: [DeclarationOrExpression] -> TypeErrorOr [DeclarationOrExpression]
typeCheck asts = do
  funcs <- functions asts
  conjunctErrors $ map (typeCheckOne funcs) asts

typeCheckOne :: FunctionMap -> DeclarationOrExpression -> TypeErrorOr DeclarationOrExpression
typeCheckOne funcs (Left d)  = typeCheckDeclaration funcs d >>= return . Left
typeCheckOne funcs (Right e) = typeCheckExp funcs Map.empty e >>= return . Right

typeCheckDeclaration :: FunctionMap -> DeclarationAst -> TypeErrorOr DeclarationAst
typeCheckDeclaration funcs declAst = do
  typedDecl <- typeCheckDeclarationAst funcs $ decl declAst
  return declAst { decl = typedDecl }

typeCheckDeclarationAst :: FunctionMap -> Declaration -> TypeErrorOr Declaration
typeCheckDeclarationAst funcs (Function sig body) =
  let var (TypedVariable varName varType) = (varName, varType)
      vars = Map.fromList $ map var (args sig)
  in typeCheckExp funcs vars body >>= return . Function sig
typeCheckDeclarationAst funcs e@(Extern _)        =  return e

functions :: [DeclarationOrExpression] -> TypeErrorOr FunctionMap
functions = functionsFromDecls . lefts

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
      typedArgs <- conjunctErrors $ map typedSubExp args
      argTypes <- conjunctErrors $ map astType typedArgs
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
                                          | otherwise             -> mismatchingCondTypes ifType elseType pos
        (condType, _, _)                                          -> invalidCondType condType pos
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

conjunctErrors :: [TypeErrorOr a] -> TypeErrorOr [a]
conjunctErrors = foldr conjunctTwoErrors (Result [])

conjunctTwoErrors :: TypeErrorOr a -> TypeErrorOr [a] -> TypeErrorOr [a]
conjunctTwoErrors a as = do
  b <- a
  bs <- as
  return $ b : bs

unOpType :: UnaryOperator -> Type -> SourcePos -> TypeErrorOr Type
unOpType op t pos = case (TypedUnOp op t) `Map.lookup` typedUnOps of
  Nothing -> unknownUnOp op t pos
  Just t  -> Result t

data TypedUnOp = TypedUnOp UnaryOperator Type
               deriving (Eq, Ord, Show)

typedUnOps :: Map.Map TypedUnOp Type
typedUnOps = Map.fromList [ (TypedUnOp UnaryPlus Types.Integer, Types.Integer)
                          , (TypedUnOp UnaryPlus Types.Double, Types.Double)
                          , (TypedUnOp UnaryMinus Types.Integer, Types.Integer)
                          , (TypedUnOp UnaryMinus Types.Double, Types.Double)
                          , (TypedUnOp BitwiseNot Types.Integer, Types.Integer)
                          , (TypedUnOp LogicalNot Types.Boolean, Types.Boolean)]

data TypedBinOp = TypedBinOp BinaryOperator Type Type
                deriving (Eq, Ord, Show)

binOpType :: BinaryOperator -> Type -> Type -> SourcePos -> TypeErrorOr Type
binOpType op s t pos = case (TypedBinOp op s t) `Map.lookup` typedBinOps of
  Nothing -> unknownBinOp op s t pos
  Just t  -> Result t

typedBinOps :: Map.Map TypedBinOp Type
typedBinOps = Map.fromList $
              map (\op -> (TypedBinOp op Types.Integer Types.Integer, Types.Integer)) intBinOps
              ++ (map (\op -> (TypedBinOp op Types.Integer Types.Integer, Types.Integer)) intDoubleBinOps)
              ++ (map (\op -> (TypedBinOp op Types.Double Types.Double, Types.Double)) intDoubleBinOps)
              ++ (map (\op -> (TypedBinOp op Types.Integer Types.Integer, Types.Boolean)) relOps)
              ++ (map (\op -> (TypedBinOp op Types.Double Types.Double, Types.Boolean)) relOps)
              ++ (map (\op -> (TypedBinOp op Types.Boolean Types.Boolean, Types.Boolean)) boolOps)

intBinOps :: [BinaryOperator]
intBinOps = [Modulo, LeftShift, RightShift, BitwiseAnd, BitwiseXor, BitwiseOr]

intDoubleBinOps :: [BinaryOperator]
intDoubleBinOps = [Times, DividedBy, Plus, Minus]

relOps :: [BinaryOperator]
relOps = [LessThan, AtMost, GreaterThan, AtLeast, EqualTo, NotEqualTo]

boolOps :: [BinaryOperator]
boolOps = [LogicalAnd, LogicalXor, LogicalOr, Implies, ImpliedBy, EquivalentTo, NotEquivalentTo]

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

invalidCondType :: Type -> SourcePos -> TypeErrorOr a
invalidCondType condType =
  typeError $ "Conditions of conditionals have to have type " ++ show Types.Boolean ++ ", found " ++ show condType

mismatchingCondTypes :: Type -> Type -> SourcePos -> TypeErrorOr a
mismatchingCondTypes ifType elseType =
  typeError $ "If and Else branch of conditionals have to have the same types, found " ++ show ifType ++ " and " ++ show elseType ++ "."

ambiguousFunction :: Signature -> SourcePos -> TypeErrorOr a
ambiguousFunction sig =
  typeError $ "Function " ++ show sig ++ " is ambiguous. A function with the same name and the same argument types already exists."

unknownType :: SourcePos -> TypeErrorOr a
unknownType = typeError "Unknown type."

typeError :: String -> SourcePos -> TypeErrorOr a
typeError msg pos = Error $ TypeError pos msg
