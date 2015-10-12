module TestUtils where

import Syntax
import Types
import Lexer
import Operators

import Data.Bifunctor
import Control.Monad
import Test.QuickCheck
import Text.Parsec.Pos
import qualified Data.Map.Strict as Map

iden0' :: [Char]
iden0' = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

iden0 :: Gen Char
iden0 = elements iden0'

idenN' :: [Char]
idenN' = iden0' ++ ['0'..'9']

idenN :: Gen [Char]
idenN = listOf $ elements idenN'

ident :: Gen Name
ident = do
  i0 <- iden0
  iN <- idenN
  return (i0:iN)

identifier :: Gen Name
identifier = ident `suchThat` isNotReserved

isNotReserved :: Name -> Bool
isNotReserved a = a `notElem` reservedNames

shrinkIdentifier :: Name -> [Name]
shrinkIdentifier = filter (\a -> not (null a) && isNotReserved a) . shrink

shrinkWithIdentifier :: Arbitrary a => Name -> a -> [(Name, a)]
shrinkWithIdentifier i b = [(i', b) | i' <- shrinkIdentifier i] ++ [(i, b') | b' <- shrink b]

testfile :: String
testfile = "<testfile>"

pos :: SourcePos
pos = (newPos testfile 0 0)

position :: Gen SourcePos
position = return pos

declarationAst :: Gen DeclarationAst
declarationAst = liftM2 DeclarationAst declaration position

declaration :: Gen Declaration
declaration = function

freeVariables :: ExpressionAst -> [TypedVariable]
freeVariables (ExpressionAst exp t _) = freeVariables' exp
  where freeVariables' :: Expression -> [TypedVariable]
        freeVariables' (BinaryOperation _ left right)   = freeVariables left ++ freeVariables right
        freeVariables' (UnaryOperation _ exp)           = freeVariables exp
        freeVariables' (Conditional cond ifExp elseExp) = freeVariables cond ++ freeVariables ifExp ++ freeVariables elseExp
        freeVariables' (Call _ args)                    = concat $ map freeVariables args
        freeVariables' (Variable a)                     = [TypedVariable a t]
        freeVariables' _                                = []

data FunctionType =
  FunctionType { name :: Name
               , argTypes :: [Type]
               , retType :: Type }
  deriving (Eq, Ord, Show)

freeFunctions :: Program -> [FunctionType]
freeFunctions = concat . map (flattenEither . first freeFunctionsDeclarationAst . second freeFunctionsExpressionAst) . program
  where flattenEither :: Either a a -> a
        flattenEither (Left a)  = a
        flattenEither (Right a) = a

freeFunctionsDeclarationAst :: DeclarationAst -> [FunctionType]
freeFunctionsDeclarationAst = freeFunctionsDeclaration . decl

freeFunctionsDeclaration :: Declaration -> [FunctionType]
freeFunctionsDeclaration (Function s body) = freeFunctionsExpressionAst body
freeFunctionsDeclaration _                 = []

freeFunctionsExpressionAst :: ExpressionAst -> [FunctionType]
freeFunctionsExpressionAst (ExpressionAst exp t _) = freeFunctionsExpressionAst' exp
  where freeFunctionsExpressionAst' :: Expression -> [FunctionType]
        freeFunctionsExpressionAst' (BinaryOperation _ left right)   = freeFunctionsExpressionAst left
                                                                       ++ freeFunctionsExpressionAst right
        freeFunctionsExpressionAst' (UnaryOperation _ exp)           = freeFunctionsExpressionAst exp
        freeFunctionsExpressionAst' (Conditional cond ifExp elseExp) = freeFunctionsExpressionAst cond
                                                                       ++ freeFunctionsExpressionAst ifExp
                                                                       ++ freeFunctionsExpressionAst elseExp
        freeFunctionsExpressionAst' (Call name args)                 = [FunctionType name (map expType args) t]
                                                                       ++ concat (map freeFunctionsExpressionAst args)
        freeFunctionsExpressionAst' _                                = []

inferSignature :: Name -> ExpressionAst -> Signature
inferSignature name exp = Signature name (freeVariables exp) (expType exp)

function :: Gen Declaration
function = do
  t <- typ
  name <- identifier
  exp <- expressionAst t
  return $ Function (inferSignature name exp) exp

expressionAst :: Type -> Gen ExpressionAst
expressionAst t = do
  e <- expression t
  p <- position
  return $ ExpressionAst e t p

typ :: Gen Type
typ = elements [Types.Boolean, Types.Integer, Types.Double]

boolean :: Gen Expression
boolean = liftM Syntax.Boolean arbitrary

integer :: Gen Expression
integer = liftM (Syntax.Integer . abs) arbitrary

double :: Gen Expression
double = liftM Syntax.Double $ elements niceDoubles
  where niceDoubles = [0.0, 0.1, 1.0, 1.1, 1e10, 1.1e10]

variable :: Gen Expression
variable = liftM Variable identifier

call :: Gen Expression
call = liftM2 Call identifier (scale pred TestUtils.args)

args :: Gen [ExpressionAst]
args = scale intRoot $ listOf arg
  where arg = typ >>= expressionAst
        intRoot = round . sqrt . fromIntegral

invert :: (Ord k, Ord v) => Map.Map k v -> Map.Map v [k]
invert m = Map.fromListWith (++) pairs
  where pairs = [(v, [k]) | (k, v) <- Map.toList m]

inverseFindWithDefault :: (Ord k, Ord v) => Map.Map v k -> k -> [v]
inverseFindWithDefault map key = Map.findWithDefault [] key (invert map)

typUnOps :: Type -> [TypedUnOp]
typUnOps = inverseFindWithDefault typedUnOps
          
typBinOps :: Type -> [TypedBinOp]
typBinOps = inverseFindWithDefault typedBinOps
          
binaryOperations :: Type -> [Gen Expression]
binaryOperations t = map binOp $ typBinOps t
  where binOp (TypedBinOp op r s) = liftM2 (BinaryOperation op) (subtree r) (subtree s)
        subtree r = scale (\n -> n `div` 2) $ expressionAst r
                                 
unaryOperations :: Type -> [Gen Expression]
unaryOperations t = map unOp $ typUnOps t
  where unOp (TypedUnOp op s) = liftM (UnaryOperation op) (subtree s)
        subtree s = scale pred $ expressionAst s

conditional :: Type -> Gen Expression
conditional t = liftM3 Conditional (subtree Types.Boolean) (subtree t) (subtree t)
  where subtree t = scale (\n -> n `div` 3) $ expressionAst t

leafExpression :: Type -> Gen Expression
leafExpression t = oneof [constant t, variable]
  where constant Types.Boolean = boolean
        constant Types.Integer = integer
        constant Types.Double  = double

innerExpression :: Type -> Gen Expression
innerExpression t =
  -- We want the same probability to get a unary operation, a binary operation, a constant, a variable, a call or a conditional
  frequency weighted
  where weighted = map ((,) weight) anyTyped ++ map ((,) numUnOps) binOps ++ map ((,) numBinOps) unOps
        anyTyped :: [Gen Expression]
        anyTyped = map ($ t) [leafExpression, leafExpression, conditional] ++ [call]
        binOps :: [Gen Expression]
        binOps = binaryOperations t
        unOps :: [Gen Expression]
        unOps = unaryOperations t
        numUnOps = length unOps
        numBinOps = length binOps
        weight = if numUnOps * numBinOps == 0 then 1 else numUnOps * numBinOps

expression :: Type -> Gen Expression
expression t = sized expression'
  where expression' :: Int -> Gen Expression
        expression' 0         = leafExpression t
        expression' n | n > 0 = innerExpression t

arbitraryTypedVariable :: Type -> Gen TypedVariable
arbitraryTypedVariable t = do
  name <- identifier
  return $ TypedVariable name t

arbitraryExtern :: FunctionType -> Gen Declaration
arbitraryExtern (FunctionType name argTypes retType) = do
  args <- sequence $ map arbitraryTypedVariable argTypes
  return $ Extern (Signature name args retType)

arbitraryProgram :: Gen Program
arbitraryProgram = do
  prog <- listOf arbitrary
  let free = freeFunctions $ Program prog
  externs <- sequence $ map (lol . arbitraryExtern) free
  return $ Program $ prog ++ externs
    where lol :: Gen Declaration -> Gen DeclarationOrExpression
          lol decl = do
            d <- decl
            return $ Left $ DeclarationAst d pos

instance Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators

instance Arbitrary ExpressionAst where
  arbitrary = typ >>= expressionAst
  shrink (ExpressionAst exp typ pos) = [ExpressionAst e typ pos | e <- shrinkExpression typ exp]

shrinkExpression :: Type -> Expression -> [Expression]
shrinkExpression _ (Syntax.Boolean b)               = map Syntax.Boolean $ shrink b
shrinkExpression _ (Syntax.Integer n)               = map Syntax.Integer $ shrink n
shrinkExpression _ (Syntax.Double d)                = map Syntax.Double $ shrink d
shrinkExpression _ (Variable v)                     = []
shrinkExpression t (BinaryOperation op left right)  = childrenWithType t [left, right]
                                                      ++ [BinaryOperation op l r | (l, r) <- shrink (left, right)]
shrinkExpression t (UnaryOperation op exp)          = childrenWithType t [exp]
                                                      ++ [UnaryOperation op e | e <- shrink exp]
shrinkExpression t (Conditional cond ifExp elseExp) = childrenWithType t [cond, ifExp, elseExp]
                                                      ++ [Conditional c i e | (c, i, e) <- shrink (cond, ifExp, elseExp)]
shrinkExpression t (Call name args)                 = childrenWithType t args
                                                      ++ [Call n a | (n, a) <- shrinkWithIdentifier name args]

childrenWithType :: Type -> [ExpressionAst] -> [Expression]
childrenWithType t = map astExp . filter (\c -> expType c == t)

instance Arbitrary DeclarationAst where
  arbitrary = declarationAst
  shrink (DeclarationAst decl pos) = [DeclarationAst d pos | d <- shrink decl]

instance Arbitrary Declaration where
  arbitrary = declaration
  shrink (Function sig body) = [Function sig b | b <- shrink body]
  shrink _                   = []

instance Arbitrary Program where
  arbitrary = arbitraryProgram
  shrink (Program []) = []
  shrink (Program (x:xs)) = map appendTail headShrinks ++ map appendHead tailShrinks
    where headShrinks = shrink x
          appendTail y = Program (y:xs)
          tailShrinks = shrink $ Program xs
          appendHead (Program ys) = Program (x:ys)

clearPositions :: Program -> Program
clearPositions = mapDeclarationAst clearPosDeclarationAst . mapExpressionAst clearPosExpressionAst
  where clearPosDeclarationAst (DeclarationAst decl _) = DeclarationAst decl pos
        clearPosExpressionAst (ExpressionAst exp typ _) = ExpressionAst exp typ pos

clearTypes :: Program -> Program
clearTypes = mapExpressionAst clearTypesExpressionAst
  where clearTypesExpressionAst (ExpressionAst exp _ pos) = ExpressionAst exp Unknown pos
