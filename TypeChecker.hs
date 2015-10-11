module TypeChecker (TypeErrorOr
                   , typeCheck) where


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
  = TypeError String
  | Result a

instance Functor TypeErrorOr where
  fmap f (TypeError s) = TypeError s
  fmap f (Result a)    = Result $ f a

instance Monad TypeErrorOr where
  TypeError e >>= f = TypeError e
  Result r    >>= f = f r
  return            = Result

typeCheck :: [DeclarationOrExpression] -> TypeErrorOr [DeclarationOrExpression]
typeCheck asts = do
  funcs <- functions asts
  conjunctErrors $ map (typeCheckOne funcs) asts

typeCheckOne :: FunctionMap -> DeclarationOrExpression -> TypeErrorOr DeclarationOrExpression
typeCheckOne funcs (Left d)  = typeCheckDeclaration funcs d >>= return . Left
typeCheckOne funcs (Right e) = typeCheckExp funcs Map.empty e >>= return . Right

typeCheckDeclaration :: FunctionMap -> Declaration -> TypeErrorOr Declaration
typeCheckDeclaration funcs (Function sig body) =
  let var (TypedVariable varName varType) = (varName, varType)
      vars = Map.fromList $ map var (args sig)
  in typeCheckExp funcs vars body >>= return . Function sig
typeCheckDeclaration funcs e@(Extern _)        =  return e

functions :: [DeclarationOrExpression] -> TypeErrorOr FunctionMap
functions = functionsFromDecls . lefts

functionsFromDecls :: [Declaration] -> TypeErrorOr FunctionMap
functionsFromDecls d = foldr addOneFunction (Result Map.empty) (map signature d)

addOneFunction :: Signature -> TypeErrorOr FunctionMap -> TypeErrorOr FunctionMap
addOneFunction sig m = do
  n <- m
  when ((functionType sig) `Map.member` n) (ambiguousFunction sig)
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
      addType (UnaryOperation op typedSubExp) (unOpType op subExpType)
    BinaryOperation op left right -> do
      typedLeft <- typedSubExp left
      typedRight <- typedSubExp right
      leftType <- astType typedLeft
      rightType <- astType typedRight
      addType (BinaryOperation op typedLeft typedRight) (binOpType op leftType rightType)
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
                                          | otherwise             -> mismatchingCondTypes ifType elseType
        (condType, _, _)                                          -> invalidCondType condType
  where typedSubExp = typeCheckExp funcs vars
        checkNotUnknown :: Type -> TypeErrorOr Type
        checkNotUnknown Unknown = unknownType
        checkNotUnknown t       = Result t
        astType :: ExpressionAst -> TypeErrorOr Type
        astType = checkNotUnknown . expType
        typedAst :: Type -> TypeErrorOr ExpressionAst
        typedAst t = do
          s <- checkNotUnknown t
          return ast { expType = s }
        addType :: Expression -> TypeErrorOr Type -> TypeErrorOr ExpressionAst
        addType e t = do
          s <- t
          return ast { astExp = e , expType = s }
        varType :: Name -> TypeErrorOr Type
        varType name = case name `Map.lookup` vars of
          Nothing -> unknownVariable name
          Just t  -> Result t
        funcType :: Name -> [Type] -> TypeErrorOr Type
        funcType name argTypes = case (FunctionType name argTypes) `Map.lookup` funcs of
          Nothing -> unknownFunction name argTypes
          Just t  -> Result t

conjunctErrors :: [TypeErrorOr a] -> TypeErrorOr [a]
conjunctErrors = foldr conjunctTwoErrors (Result [])

conjunctTwoErrors :: TypeErrorOr a -> TypeErrorOr [a] -> TypeErrorOr [a]
conjunctTwoErrors a as = do
  b <- a
  bs <- as
  return $ b : bs

unOpType :: UnaryOperator -> Type -> TypeErrorOr Type
unOpType op t = case (TypedUnOp op t) `Map.lookup` typedUnOps of
  Nothing -> unknownUnOp op t
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

binOpType :: BinaryOperator -> Type -> Type -> TypeErrorOr Type
binOpType op s t = case (TypedBinOp op s t) `Map.lookup` typedBinOps of
  Nothing -> unknownBinOp op s t
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

unknownUnOp :: UnaryOperator -> Type -> TypeErrorOr a
unknownUnOp op t =
  TypeError $ "Unknown operator " ++ show op ++ " for expression of type " ++ show t ++ "."

unknownBinOp :: BinaryOperator -> Type -> Type -> TypeErrorOr a
unknownBinOp op s t =
  TypeError $ "Unknown operator " ++ show op ++ " for expressions of type " ++ show s ++ " and " ++ show t ++ "."

unknownVariable :: Name -> TypeErrorOr a
unknownVariable name = TypeError $ "Unknown variable " ++ name ++ "."

unknownFunction :: Name -> [Type] -> TypeErrorOr a
unknownFunction name argTypes =
  TypeError $ "Unknown function " ++ name ++ " for argument types " ++ show argTypes ++ "."

invalidCondType :: Type -> TypeErrorOr a
invalidCondType condType =
  TypeError $ "Conditions of conditionals have to have type " ++ show Types.Boolean ++ ", found " ++ show condType

mismatchingCondTypes :: Type -> Type -> TypeErrorOr a
mismatchingCondTypes ifType elseType =
  TypeError $ "If and Else branch of conditionals have to have the same types, found " ++ show ifType ++ " and " ++ show elseType ++ "."

ambiguousFunction :: Signature -> TypeErrorOr a
ambiguousFunction sig =
  TypeError $ "Function " ++ show sig ++ " is ambiguous. A function with the same name and the same argument types already exists."

unknownType :: TypeErrorOr a
unknownType =
  TypeError "Unknown type."
