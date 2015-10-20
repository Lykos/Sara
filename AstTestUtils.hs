module AstTestUtils where

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

iden0' :: String
iden0' = ['a'..'z'] ++ ['A'..'Z'] ++ "_"

iden0 :: Gen Char
iden0 = elements iden0'

idenN' :: String
idenN' = iden0' ++ ['0'..'9']

idenN :: Gen String
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
  pure <- elements [True, False]
  let constructor = if pure then Function else Method
  t <- typ
  name <- identifier
  exp <- expressionAst pure t
  return $ constructor (inferSignature name exp) exp

expressionAst :: Bool -> Type -> Gen ExpressionAst
expressionAst pure t = do
  e <- expression pure t
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

call :: Bool -> Gen Expression
call pure = liftM2 Call identifier (scale pred $ AstTestUtils.args pure)

args :: Bool -> Gen [ExpressionAst]
args pure = scale intRoot $ listOf arg
  where arg = typ >>= expressionAst pure

intRoot :: Int -> Int
intRoot = round . sqrt . fromIntegral

invert :: (Ord k, Ord v) => Map.Map k v -> Map.Map v [k]
invert m = Map.fromListWith (++) pairs
  where pairs = [(v, [k]) | (k, v) <- Map.toList m]

inverseFindWithDefault :: (Ord k, Ord v) => Map.Map v k -> k -> [v]
inverseFindWithDefault map key = Map.findWithDefault [] key (invert map)

typUnOps :: Type -> [TypedUnOp]
typUnOps = inverseFindWithDefault typedUnOps
          
typBinOps :: Bool -> Type -> [TypedBinOp]
typBinOps pure t = filter (\(TypedBinOp op _ _) -> not pure || op /= Assign) (inverseFindWithDefault typedBinOps t)
          
binaryOperations :: Bool -> Type -> [Gen Expression]
binaryOperations pure t = map binOp $ typBinOps pure t
  where binOp (TypedBinOp Assign r s) = do
          var <- variable r
          let var' = ExpressionAst var r pos
          val <- subtree s
          return $ BinaryOperation Assign var' val
        binOp (TypedBinOp op r s)     = liftM2 (BinaryOperation op) (subtree r) (subtree s)
        subtree r = scale (`div` 2) $ expressionAst pure r
                                 
unaryOperations :: Bool -> Type -> [Gen Expression]
unaryOperations pure t = map unOp $ typUnOps t
  where unOp (TypedUnOp op s) = liftM (UnaryOperation op) (subtree s)
        subtree s = scale pred $ expressionAst pure s

conditional :: Bool -> Type -> Gen Expression
conditional pure t = liftM3 Conditional (subtree Types.Boolean) (subtree t) (subtree t)
  where subtree t = scale (`div` 3) $ expressionAst pure t

block :: Bool -> Type -> Gen Expression
block pure t = do
  t' <- typ
  stmts <- scale intRoot $ listOf $ expressionAst pure t'
  expr <- scale intRoot $ expressionAst pure t
  return $ Block stmts expr

while :: Gen Expression
while = liftM2 While (subtree Types.Boolean) (typ >>= subtree)
  where subtree t = scale (`div` 2) $ expressionAst False t

leafExpression :: Type -> Gen Expression
leafExpression t = oneof [constant t, variable t]
  where constant Types.Boolean = boolean
        constant Types.Integer = integer
        constant Types.Double  = double
        constant Types.Unit    = return Syntax.Unit

innerExpression :: Bool -> Type -> Gen Expression
innerExpression pure t =
  -- We want the same probability to get a unary operation, a binary operation, a constant, a variable, a call or a conditional
  frequency weighted
  where weighted = map ((,) weight) anyTyped ++ map ((,) numUnOps) binOps ++ map ((,) numBinOps) unOps
        anyTyped :: [Gen Expression]
        anyTyped = map ($ t) [leafExpression, leafExpression, conditional pure, block pure] ++ [call pure] ++ maybeWhile
        maybeWhile :: [Gen Expression]
        maybeWhile = [while | not pure && t == Types.Unit]
        binOps :: [Gen Expression]
        binOps = binaryOperations pure t
        unOps :: [Gen Expression]
        unOps = unaryOperations pure t
        numUnOps = length unOps
        numBinOps = length binOps
        weight = if numUnOps * numBinOps == 0 then 1 else numUnOps * numBinOps

expression :: Bool -> Type -> Gen Expression
expression pure t = sized expression'
  where expression' :: Int -> Gen Expression
        expression' 0         = leafExpression t
        expression' n | n > 0 = innerExpression pure t

arbitraryTypedVariable :: Type -> Gen TypedVariable
arbitraryTypedVariable t = do
  name <- identifier
  return $ TypedVariable name t pos

arbitraryExtern :: FunctionType -> Gen Declaration
arbitraryExtern (FunctionType name argTypes retType) = do
  args <- mapM arbitraryTypedVariable argTypes
  return $ Extern (Signature name args retType)

fixName :: [Name] -> Name -> Name
fixName names name | name `elem` names = fixName names (name ++ "0")
                   | otherwise         = name

arbitraryProgram :: Gen Program
arbitraryProgram = do
  prog <- liftM Program $ listOf arbitrary
  let prog' = fixFunctionNameClashes prog
  let free = calledFunctions prog'
  externs <- mapM (addPosition . arbitraryExtern) free
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
            name' <- fixName' name
            return $ Signature name' argTypes typ
          fixExpressionAst :: ExpressionAst -> State [Name] ExpressionAst
          fixExpressionAst (ExpressionAst e typ pos) = do
            e' <- fixExpression e
            return $ ExpressionAst e' typ pos
          fixExpression :: Expression -> State [Name] Expression
          fixExpression (Call name args) = do
            name' <- fixName' name
            return $ Call name' args
          fixExpression e                = return e
          fixName' :: Name -> State [Name] Name
          fixName' name = do
            names <- get
            let name' = fixName names name
            put (name':names)
            return name'

trivial :: Type -> Expression
trivial Types.Boolean = Syntax.Boolean False
trivial Types.Integer = Syntax.Integer 0
trivial Types.Double  = Syntax.Double 0.0

instance Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators

shrinkExpressionAst :: ExpressionAst -> [ExpressionAst]
shrinkExpressionAst (ExpressionAst exp typ pos) = [ExpressionAst e typ pos | e <- shrinkExpression typ exp]

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
shrinkExpression t (Block stmts exp)                = shrinkBlock t stmts exp
shrinkExpression _ (While cond body)                = shrinkWhile cond body

childrenWithType :: Type -> [ExpressionAst] -> [Expression]
childrenWithType t = map astExp . filter (\c -> expType c == t)

shrinkBlock :: Type -> [ExpressionAst] -> ExpressionAst -> [Expression]
shrinkBlock t stmts exp = [astExp exp] ++ withoutExp ++ [Block s e | (s, e) <- shrink (stmts, exp)]
  where withoutExp :: [Expression]
        withoutExp = if null stmts then
                       []
                     else
                       let exp' = last stmts in
                         if expType exp' /= t then [] else [Block (init stmts) exp']

-- TODO Throw the expression away
shrinkWhile :: ExpressionAst -> ExpressionAst -> [Expression]
shrinkWhile stmts body = [While c b | (c, b) <- shrink (stmts, body)]

shrinkTypedVariable :: TypedVariable -> [TypedVariable]
shrinkTypedVariable (TypedVariable var typ _) = [TypedVariable v typ pos | v <- shrinkIdentifier var]

shrinkSignature :: [TypedVariable] -> Signature -> [Signature]
shrinkSignature free (Signature name args typ) = [Signature name a typ | a <- shrinkArgTypes args]
  where shrinkArgTypes :: [TypedVariable] -> [[TypedVariable]]
        shrinkArgTypes []                     = []
        shrinkArgTypes (x:xs) | x `elem` free = [x:ys | ys <- shrinkArgTypes xs]
                              | otherwise     = xs : [y : xs | y <- shrinkTypedVariable x]
                                                ++ [x:ys | ys <- shrinkArgTypes xs]

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
                isRemovableSignature (Signature name args retType) = FunctionType name (map varType args) retType `notElem` funcs
                appendTail y = Program (y:xs)
                tailShrinks = shrinkProgram' funcs $ Program xs
                appendHead (Program ys) = Program (x:ys)

instance Arbitrary ExpressionAst where
  arbitrary = typ >>= expressionAst False
  shrink = shrinkExpressionAst

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
