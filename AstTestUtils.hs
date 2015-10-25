module AstTestUtils (
  clearTypes
  , identifier
  , pos
  , clearPositions
  , testfile
  , inferSignature
  , completeProgram
  , PureExpression(..)
  ) where

import Syntax
import Types
import Lexer
import Operators
import AstUtils
import Reporter
import qualified Syntax as S
import qualified Types as T

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

addPosition :: Gen (SourcePos -> a) -> Gen a
addPosition gen = do
  gen' <- gen
  return $ gen' pos

addTypePos :: Type -> Gen UntypedExpression -> Gen Expression
addTypePos t gen = do
  gen' <- gen
  return $ gen' t pos

position :: Gen SourcePos
position = return pos

declaration :: Gen Declaration
declaration = addPosition function

freeVariables :: Expression -> [TypedVariable]
freeVariables = foldMapExpression freeVariable
  where freeVariable :: Expression -> [TypedVariable]
        freeVariable (Variable a t _) = [TypedVariable a t pos]
        freeVariable _                = []

data FunctionType =
  FunctionType { pure :: Bool
               , name :: Name
               , argTypes :: [Type]
               , retType :: Type }
  deriving (Eq, Ord, Show)

calledFunctions :: Program -> [FunctionType]
calledFunctions = foldMapExpressions calledFunctionsExpression
  where calledFunctionsExpression :: Expression -> [FunctionType]
        calledFunctionsExpression (Call name args typ _) =
          [FunctionType True name (map expType args) typ]
        calledFunctionsExpression _                      = []

inferSignature :: Bool -> Name -> Expression -> Signature
inferSignature pure name exp = Signature pure name (freeVariables exp) (S.typ exp) pos

type UnpositionedDeclaration = SourcePos -> Declaration

function :: Gen UnpositionedDeclaration
function = do
  pure <- elements [True, False]
  t <- AstTestUtils.typ
  name <- identifier
  exp <- expression pure t
  return $ Function (inferSignature pure name exp) exp

typ :: Gen Type
typ = elements [T.Unit, T.Boolean, T.Integer, T.Double]

type UntypedExpression = Type -> SourcePos -> Expression

boolean :: Gen UntypedExpression
boolean = liftM S.Boolean arbitrary

integer :: Gen UntypedExpression
integer = liftM (S.Integer . abs) arbitrary

double :: Gen UntypedExpression
double = liftM S.Double $ elements niceDoubles
  where niceDoubles = [0.0, 0.1, 1.0, 1.1, 1e10, 1.1e10]

-- We add the type to the variable name in order to avoid name clashes.
variable :: Type -> Gen UntypedExpression
variable t = do
  id <- identifier
  return $ Variable $ id ++ show t

call :: Bool -> Type -> Gen UntypedExpression
call pure t = do
  name <- identifier
  args <- (scale pred $ AstTestUtils.args pure)
  return $ Call name args

args :: Bool -> Gen [Expression]
args pure = scale intRoot $ listOf arg
  where arg = AstTestUtils.typ >>= expression pure

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
          
binaryOperations :: Bool -> Type -> [Gen UntypedExpression]
binaryOperations pure t = map binOp $ typBinOps pure t
  where binOp :: TypedBinOp -> Gen UntypedExpression
        binOp (TypedBinOp Assign r s) = liftM2 (BinaryOperation Assign) var (subtree s)
          where var = addTypePos r (variable r)
        binOp (TypedBinOp op r s)     = do
          left <- subtree r
          right <- subtree s
          return $ BinaryOperation op left right
        subtree r = scale (`div` 2) $ expression pure r
                                 
unaryOperations :: Bool -> Type -> [Gen UntypedExpression]
unaryOperations pure t = map unOp $ typUnOps t
  where unOp (TypedUnOp op s) = liftM (UnaryOperation op) (subtree s)
        subtree s = scale pred $ expression pure s

conditional :: Bool -> Type -> Gen UntypedExpression
conditional pure t = liftM3 Conditional (subtree T.Boolean) (subtree t) (subtree t)
  where subtree t = scale (`div` 3) $ expression pure t

block :: Bool -> Type -> Gen UntypedExpression
block pure t = liftM2 Block stmts exp
  where stmts = scale intRoot $ listOf $ (AstTestUtils.typ >>= expression pure)
        exp = scale intRoot $ expression pure t

while :: Gen UntypedExpression
while = liftM2 While (subtree T.Boolean) (AstTestUtils.typ >>= subtree)
  where subtree t = scale (`div` 2) $ expression False t

leafExpression :: Type -> Gen UntypedExpression
leafExpression t = oneof [constant t, variable t]
  where constant T.Boolean = boolean
        constant T.Integer = integer
        constant T.Double  = double
        constant T.Unit    = return $ S.Unit

innerExpression :: Bool -> Type -> Gen UntypedExpression
innerExpression pure t =
  -- We want the same probability to get a unary operation, a binary operation, a constant, a variable, a call or a conditional
  frequency weighted
  where weighted = map ((,) weight) anyTyped ++ map ((,) numUnOps) binOps ++ map ((,) numBinOps) unOps
        anyTyped :: [Gen UntypedExpression]
        anyTyped = map ($ t) [leafExpression, leafExpression, conditional pure, block pure, call pure] ++ maybeWhile
        maybeWhile :: [Gen UntypedExpression]
        maybeWhile = [while | not pure && t == T.Unit]
        binOps :: [Gen UntypedExpression]
        binOps = binaryOperations pure t
        unOps :: [Gen UntypedExpression]
        unOps = unaryOperations pure t
        numUnOps = length unOps
        numBinOps = length binOps
        weight = if numUnOps * numBinOps == 0 then 1 else numUnOps * numBinOps

expression :: Bool -> Type -> Gen Expression
expression pure t = addTypePos t $ sized expression'
  where expression' :: Int -> Gen UntypedExpression
        expression' 0         = leafExpression t
        expression' n | n > 0 = innerExpression pure t

arbitraryTypedVariable :: Type -> Gen TypedVariable
arbitraryTypedVariable t = do
  name <- identifier
  return $ TypedVariable name t pos

arbitraryExtern :: FunctionType -> Gen Declaration
arbitraryExtern (FunctionType pure name argTypes retType) = do
  args <- mapM arbitraryTypedVariable argTypes
  return $ Extern (Signature pure name args retType pos) pos

fixName :: [Name] -> Name -> Name
fixName names name | name `elem` names = fixName names (name ++ "0")
                   | otherwise         = name

arbitraryProgram :: Gen Program
arbitraryProgram = do
  decls <- listOf arbitrary
  completeProgram decls

completeProgram :: [Declaration] -> Gen Program
completeProgram decls = do
  let prog = Program decls pos
  let prog' = fixFunctionNameClashes prog
  let free = calledFunctions prog'
  externs <- mapM arbitraryExtern free
  return $ prog' { program = program prog' ++ externs }
    where fixFunctionNameClashes :: Program -> Program
          fixFunctionNameClashes prog = evalState (fixFunctionNameClashes' prog) []
          fixFunctionNameClashes' :: Program -> State [Name] Program
          fixFunctionNameClashes' prog =
            transformSignatures fixSignature prog >>= transformExpressions fixExpression
          fixSignature :: Signature -> State [Name] Signature
          fixSignature s@Signature{ S.sigName = n } = do
            n' <- fixName' n
            return $ s{ S.sigName = n' }
          fixExpression :: Expression -> State [Name] Expression
          fixExpression c@Call{ S.expName = n } = do
            n' <- fixName' n
            return $ c{ S.expName = n' }
          fixExpression e                    = return e
          fixName' :: Name -> State [Name] Name
          fixName' name = do
            names <- get
            let name' = fixName names name
            put (name' : names)
            return name'

trivial :: Type -> SourcePos -> Expression
trivial t = trivial' t t
  where trivial' T.Boolean = S.Boolean False
        trivial' T.Integer = S.Integer 0
        trivial' T.Double  = S.Double 0.0

instance Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators

shrinkExpression :: Expression -> [Expression]
shrinkExpression b@S.Boolean{ boolValue = val }       = [b{ boolValue = v } | v <- shrink val]
shrinkExpression n@S.Integer{ intValue = val }        = [n{ intValue = v } | v <- shrink val]
shrinkExpression d@S.Double{ doubleValue = val }      = [d{ doubleValue = v } | v <- shrink val]
shrinkExpression (Variable v t p)                     = [trivial t p]
shrinkExpression (BinaryOperation op left right t p)  = childrenWithType t [left, right]
                                                        ++ [BinaryOperation op l r t p | (l, r) <- shrink (left, right)]
shrinkExpression (UnaryOperation op exp t p)          = childrenWithType t [exp]
                                                        ++ [UnaryOperation op e t p | e <- shrink exp]
shrinkExpression (Call name args t p)                 = childrenWithType t args
                                                        ++ [Call name a t p | a <- shrinkArgs args]
  where shrinkArgs :: [Expression] -> [[Expression]]
        shrinkArgs []     = []
        shrinkArgs (x:xs) = [y : xs | y <- shrink x] ++ [x : ys | ys <- shrinkArgs xs]
shrinkExpression (Conditional cond ifExp elseExp t p) = childrenWithType t [cond, ifExp, elseExp]
                                                        ++ [Conditional c i e t p | (c, i, e) <- shrink (cond, ifExp, elseExp)]
shrinkExpression (Block stmts exp t p)                = [exp]
                                                        ++ childrenWithType t stmts
                                                        ++ [Block (init stmts) (last stmts) t p | not (null stmts), S.typ (last stmts) == t]
                                                        ++ [Block s e t p | (s, e) <- shrink (stmts, exp)]
shrinkExpression (While cond body t p)                = While cond (S.Unit T.Unit p) t p
                                                        : [While c b t p | (c, b) <- shrink (cond, body)]

childrenWithType :: Type -> [Expression] -> [Expression]
childrenWithType t = filter (\c -> S.typ c == t)

shrinkTypedVariable :: TypedVariable -> [TypedVariable]
shrinkTypedVariable (TypedVariable var typ p) = [TypedVariable v typ p | v <- shrinkIdentifier var]

shrinkSignature :: [TypedVariable] -> Signature -> [Signature]
shrinkSignature free (Signature pure name args typ p) = [Signature pure name a typ p | a <- shrinkArgTypes args]
  where shrinkArgTypes :: [TypedVariable] -> [[TypedVariable]]
        shrinkArgTypes []                     = []
        shrinkArgTypes (x:xs) | x `elem` free = [x : ys | ys <- shrinkArgTypes xs]
                              | otherwise     = xs : [y : xs | y <- shrinkTypedVariable x]
                                                ++ [x : ys | ys <- shrinkArgTypes xs]

shrinkProgram :: Program -> [Program]
shrinkProgram p = shrinkProgram' (calledFunctions p) p
  where pos = S.position p
        shrinkProgram' :: [FunctionType] -> Program -> [Program]
        shrinkProgram' funcs (Program [] _)     = []
        shrinkProgram' funcs (Program (x:xs) _) = headRemovals ++ map appendTail headShrinks ++ map appendHead tailShrinks
          where headShrinks = shrink x
                headRemovals :: [Program]
                headRemovals = if isRemovable x then Program xs pos : [Program (d : xs) pos | d <- shrinkSig x] else []
                shrinkSig :: Declaration -> [Declaration]
                shrinkSig (Function sig body p) = [Function s body p | s <- shrinkSignature (freeVariables body) sig]
                shrinkSig (Extern sig p)          = [Extern s p | s <- shrinkSignature [] sig]
                isRemovable :: Declaration -> Bool
                isRemovable d = isRemovableSignature $ signature d
                isRemovableSignature :: Signature -> Bool
                isRemovableSignature (Signature pure name args retType _) = FunctionType pure name (map varType args) retType `notElem` funcs
                appendTail :: Declaration -> Program
                appendTail y = Program (y:xs) pos
                tailShrinks :: [Program]
                tailShrinks = shrinkProgram' funcs $ Program xs pos
                appendHead :: Program -> Program
                appendHead (Program ys p) = Program (x : ys) p

newtype PureExpression
  = PureExpression { runPureExpression :: Expression }
  deriving (Eq, Ord, Show)

instance Arbitrary Expression where
  arbitrary = AstTestUtils.typ >>= expression False
  shrink = shrinkExpression

instance Arbitrary PureExpression where
  arbitrary = AstTestUtils.typ >>= liftM PureExpression . expression True
  shrink = map PureExpression . shrinkExpression . runPureExpression

instance Arbitrary Declaration where
  arbitrary = declaration
  shrink (Function sig body p) = [Function sig b p | b <- shrink body]
  shrink _                     = []

instance Arbitrary Program where
  arbitrary = arbitraryProgram
  shrink = shrinkProgram

instance Arbitrary Type where
  arbitrary = AstTestUtils.typ

clearPositions :: Program -> Program
clearPositions p = clearPosProgram p{ progPos = pos }
  where clearPosProgram = mapDeclarations clearPosDeclaration . mapExpressions clearPosExpression . mapSignatures clearPosSignature
        clearPosDeclaration d   = d{ declPos = pos }
        clearPosExpression e    = e{ expPos = pos }
        clearPosSignature s     = let args' = map clearPosTypedVariable $ S.args s in s{ sigPos = pos, S.args = args' }
        clearPosTypedVariable v = v{ varPos = pos }

clearTypes :: Program -> Program
clearTypes = mapExpressions clearTypes
  where clearTypes exp = exp{ expType = Unknown }
