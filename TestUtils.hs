module TestUtils where

import Syntax
import Types
import Lexer
import Operators
import AstUtils
import Reporter

import Control.Monad.State
import qualified Data.Set as Set
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
isNotReserved a = a /= "main" && a `notElem` reservedNames

shrinkIdentifier :: Name -> [Name]
shrinkIdentifier = filter (\a -> not (null a) && isNotReserved a) . shrink

shrinkWithIdentifier :: Arbitrary a => Name -> a -> [(Name, a)]
shrinkWithIdentifier i b = [(i', b) | i' <- shrinkIdentifier i] ++ [(i, b') | b' <- shrink b]

testfile :: String
testfile = "<testfile>"

pos :: SourcePos
pos = newPos testfile 0 0

position :: Gen SourcePos
position = return pos

declarationAst :: Gen DeclarationAst
declarationAst = liftM2 DeclarationAst declaration position

declaration :: Gen Declaration
declaration = functionOrMethod

freeVariables :: ExpressionAst -> [TypedVariable]
freeVariables = foldMapExpressionAst freeVariable
  where freeVariable :: ExpressionAst -> [TypedVariable]
        freeVariable (ExpressionAst (Variable a) t _) = [TypedVariable a t pos]
        freeVariable _                                = []

data FunctionType =
  FunctionType { name :: Name
               , argTypes :: [Type]
               , retType :: Type }
  deriving (Eq, Ord, Show)

calledFunctions :: Program -> [FunctionType]
calledFunctions = foldMapExpressionAsts calledFunctionsExpression
  where calledFunctionsExpression :: ExpressionAst -> [FunctionType]
        calledFunctionsExpression (ExpressionAst (Call name args) typ _) =
          [FunctionType name (map expType args) typ]
        calledFunctionsExpression _                                      = []

inferSignature :: Name -> ExpressionAst -> Signature
inferSignature name exp = Signature name (freeVariables exp) (expType exp)

functionOrMethod :: Gen Declaration
functionOrMethod = do
  constructor <- elements [Function, Method]
  t <- typ
  name <- identifier
  exp <- expressionAst t
  return $ constructor (inferSignature name exp) exp

expressionAst :: Type -> Gen ExpressionAst
expressionAst t = do
  e <- expression t
  p <- position
  return $ ExpressionAst e t p

typ :: Gen Type
typ = elements [Types.Unit, Types.Boolean, Types.Integer, Types.Double]

boolean :: Gen Expression
boolean = liftM Syntax.Boolean arbitrary

integer :: Gen Expression
integer = liftM (Syntax.Integer . abs) arbitrary

double :: Gen Expression
double = liftM Syntax.Double $ elements niceDoubles
  where niceDoubles = [0.0, 0.1, 1.0, 1.1, 1e10, 1.1e10]

-- We add the type to the variable name in order to avoid name clashes.
variable :: Type -> Gen Expression
variable t = do
  id <- identifier
  return $ Variable $ id ++ show t

call :: Gen Expression
call = liftM2 Call identifier (scale pred TestUtils.args)

args :: Gen [ExpressionAst]
args = scale intRoot $ listOf arg
  where arg = typ >>= expressionAst

intRoot :: Int -> Int
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

block :: Type -> Gen Expression
block t = do
  t' <- typ
  stmts <- scale intRoot $ listOf $ expressionAst t'
  expr <- scale intRoot $ expressionAst t
  return $ Block stmts expr

leafExpression :: Type -> Gen Expression
leafExpression t = oneof [constant t, variable t]
  where constant Types.Boolean = boolean
        constant Types.Integer = integer
        constant Types.Double  = double
        constant Types.Unit    = return Syntax.Unit

innerExpression :: Type -> Gen Expression
innerExpression t =
  -- We want the same probability to get a unary operation, a binary operation, a constant, a variable, a call or a conditional
  frequency weighted
  where weighted = map ((,) weight) anyTyped ++ map ((,) numUnOps) binOps ++ map ((,) numBinOps) unOps
        anyTyped :: [Gen Expression]
        anyTyped = map ($ t) [leafExpression, leafExpression, conditional, block] ++ [call]
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
  return $ TypedVariable name t pos

arbitraryExtern :: FunctionType -> Gen Declaration
arbitraryExtern (FunctionType name argTypes retType) = do
  args <- sequence $ map arbitraryTypedVariable argTypes
  return $ Extern (Signature name args retType)

fixName :: [Name] -> Name -> Name
fixName names name | name `elem` names = fixName names (name ++ "0")
                   | otherwise         = name

arbitraryProgram :: Gen Program
arbitraryProgram = do
  prog <- liftM Program $ listOf arbitrary
  let prog' = fixFunctionNameClashes prog
  let free = calledFunctions prog'
  externs <- sequence $ map (addPosition . arbitraryExtern) free
  return $ Program $ program prog' ++ externs
    where addPosition :: Gen Declaration -> Gen DeclarationAst
          addPosition decl = liftM2 DeclarationAst decl position
          fixFunctionNameClashes :: Program -> Program
          fixFunctionNameClashes prog = evalState (fixFunctionNameClashes' prog) []
          fixFunctionNameClashes' :: Program -> State [Name] Program
          fixFunctionNameClashes' prog =
            transformSignatures fixSignature prog >>= transformExpressionAsts fixExpressionAst
          fixSignature :: Signature -> State [Name] Signature
          fixSignature (Signature name argTypes typ) = do
            names <- get
            let name' = fixName names name
            put (name':names)
            return $ Signature name' argTypes typ
          fixExpressionAst :: ExpressionAst -> State [Name] ExpressionAst
          fixExpressionAst (ExpressionAst e typ pos) = do
            e' <- fixExpression e
            return $ ExpressionAst e' typ pos
          fixExpression :: Expression -> State [Name] Expression
          fixExpression (Call name args) = do
            names <- get
            let name' = fixName names name
            put (name':names)
            return $ Call name' args
          fixExpression e                = return e

trivial :: Type -> Expression
trivial Types.Boolean = Syntax.Boolean False
trivial Types.Integer = Syntax.Integer 0
trivial Types.Double  = Syntax.Double 0.0

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
shrinkExpression t (Variable v)                     = [trivial t]
shrinkExpression t (BinaryOperation op left right)  = childrenWithType t [left, right]
                                                      ++ [BinaryOperation op l r | (l, r) <- shrink (left, right)]
shrinkExpression t (UnaryOperation op exp)          = childrenWithType t [exp]
                                                      ++ [UnaryOperation op e | e <- shrink exp]
shrinkExpression t (Call name args)                 = childrenWithType t args
                                                      ++ [Call name a | a <- shrinkArgs args]
  where shrinkArgs :: [ExpressionAst] -> [[ExpressionAst]]
        shrinkArgs []     = []
        shrinkArgs (x:xs) = [y : xs | y <- shrink x] ++ [x:ys | ys <- shrinkArgs xs]
shrinkExpression t (Conditional cond ifExp elseExp) = childrenWithType t [cond, ifExp, elseExp]
                                                      ++ [Conditional c i e | (c, i, e) <- shrink (cond, ifExp, elseExp)]
shrinkExpression t (Block stmts exp)                = withoutExp ++ [Block s e | (s, e) <- shrink (stmts, exp)]
  where withoutExp :: [Expression]
        withoutExp = let exp' = last stmts in
          if expType exp' == t then [Block (init stmts) exp'] else []

childrenWithType :: Type -> [ExpressionAst] -> [Expression]
childrenWithType t = map astExp . filter (\c -> expType c == t)

shrinkTypedVariable :: TypedVariable -> [TypedVariable]
shrinkTypedVariable (TypedVariable var typ _) = [TypedVariable v typ pos | v <- shrinkIdentifier var]

shrinkSignature :: [TypedVariable] -> Signature -> [Signature]
shrinkSignature free (Signature name args typ) = [Signature name a typ | a <- shrinkArgTypes args]
  where shrinkArgTypes :: [TypedVariable] -> [[TypedVariable]]
        shrinkArgTypes []                     = []
        shrinkArgTypes (x:xs) | x `elem` free = [(x:ys) | ys <- shrinkArgTypes xs]
                              | otherwise     = xs : [y : xs | y <- shrinkTypedVariable x]
                                                ++ [(x:ys) | ys <- shrinkArgTypes xs]

shrinkProgram :: Program -> [Program]
shrinkProgram p = shrinkProgram' (calledFunctions p) p
  where shrinkProgram' funcs (Program [])     = []
        shrinkProgram' funcs (Program (x:xs)) = headRemovals ++ map appendTail headShrinks ++ map appendHead tailShrinks
          where headShrinks = shrink x
                headRemovals :: [Program]
                headRemovals = if isRemovable x then Program xs : [Program (d : xs) | d <- shrinkSig x] else []
                shrinkSig :: DeclarationAst -> [DeclarationAst]
                shrinkSig (DeclarationAst decl pos) = [DeclarationAst d pos | d <- shrinkSig' decl]
                shrinkSig' :: Declaration -> [Declaration]
                shrinkSig' (Function sig body) = [Function s body | s <- shrinkSignature (freeVariables body) sig]
                shrinkSig' (Method sig body)   = [Method s body | s <- shrinkSignature (freeVariables body) sig]
                shrinkSig' (Extern s)          = map Extern $ shrinkSignature [] s
                isRemovable :: DeclarationAst -> Bool
                isRemovable (DeclarationAst d _) = isRemovableSignature $ signature d
                isRemovableSignature (Signature name args retType) = not (FunctionType name (map varType args) retType `elem` funcs)
                appendTail y = Program (y:xs)
                tailShrinks = shrinkProgram' funcs $ Program xs
                appendHead (Program ys) = Program (x:ys)

instance Arbitrary DeclarationAst where
  arbitrary = declarationAst
  shrink (DeclarationAst decl pos) = [DeclarationAst d pos | d <- shrink decl]

instance Arbitrary Declaration where
  arbitrary = declaration
  shrink (Function sig body) = [Function sig b | b <- shrink body]
  shrink _                   = []

instance Arbitrary Program where
  arbitrary = arbitraryProgram
  shrink = shrinkProgram

clearPositions :: Program -> Program
clearPositions = mapDeclarationAsts clearPosDeclarationAst . mapExpressionAsts clearPosExpressionAst . mapSignatures clearPosSignature
  where clearPosDeclarationAst (DeclarationAst decl _)      = DeclarationAst decl pos
        clearPosExpressionAst (ExpressionAst exp typ _)     = ExpressionAst exp typ pos
        clearPosSignature (Signature name argTypes retType) = Signature name (map clearPosTypedVariable argTypes) retType
        clearPosTypedVariable (TypedVariable name typ _)    = TypedVariable name typ pos

clearTypes :: Program -> Program
clearTypes = mapExpressionAsts clearTypes
  where clearTypes (ExpressionAst exp _ pos) = ExpressionAst exp Unknown pos

reporter :: Reporter
reporter = Reporter nop nop nop nop nop
           where nop :: a -> IO ()
                 nop a = return ()
