{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Sara.AstTestUtils (
  clearTypes
  , identifier
  , pos
  , clearPositions
  , testfile
  , inferSignature
  , completeProgram
  , mkNodeMeta
  , PureExpression(..)
  ) where

import Sara.Syntax
import Sara.Types
import Sara.Lexer
import Sara.Meta
import Sara.Operators
import Sara.AstUtils
import qualified Sara.Syntax as S
import qualified Sara.Types as T

import Control.Monad.State
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

testfile :: String
testfile = "<testfile>"

pos :: SourcePos
pos = newPos testfile 0 0

addNodeMeta :: Gen (ParserNodeMeta -> a) -> Gen a
addNodeMeta gen = gen <*> pure mkNodeMeta

mkExpMeta :: Type -> ExpMeta
mkExpMeta t = (TypeCheckerExpressionMeta t, ParserNodeMeta pos)

addExpMeta :: Type -> Gen UntypedExpression -> Gen TypeCheckerExpression
addExpMeta t gen = gen <*> pure (mkExpMeta t)

declaration :: Gen TypeCheckerDeclaration
declaration = addNodeMeta function

freeVariables :: TypeCheckerExpression -> [ParserTypedVariable]
freeVariables = foldMapExpression freeVariable
  where freeVariable :: TypeCheckerExpression -> [ParserTypedVariable]
        freeVariable v@(Variable a _ _) = [TypedVariable a (expressionTyp v) ((), mkNodeMeta)]
        freeVariable _                  = []

data FunctionType =
  FunctionType Bool Name [Type] Type
  deriving (Eq, Ord, Show)

calledFunctions :: TypeCheckerProgram -> [FunctionType]
calledFunctions = foldMapExpressions calledFunctionsExpression
  where calledFunctionsExpression :: TypeCheckerExpression -> [FunctionType]
        calledFunctionsExpression c@(Call name args _ _) =
          [FunctionType True name (map expressionTyp args) (expressionTyp c)]
        calledFunctionsExpression _                      = []

inferSignature :: Bool -> Name -> [TypeCheckerExpression] -> [TypeCheckerExpression] -> TypeCheckerExpression -> TypeCheckerSignature
inferSignature pure name precs posts exp = Signature pure name freeVars (expressionTyp exp) precs posts ((), mkNodeMeta)
  where freeVars = concatMap freeVariables precs ++ concatMap freeVariables posts ++ freeVariables exp

type UnpositionedDeclaration = ParserNodeMeta -> TypeCheckerDeclaration

function :: Gen UnpositionedDeclaration
function = do
  pure <- elements [True, False]
  t <- Sara.AstTestUtils.typ
  name <- identifier
  exp <- expression pure t
  return $ Function (inferSignature pure name [] [] exp) exp

typ :: Gen Type
typ = elements [T.Unit, T.Boolean, T.Integer, T.Double]

type ExpMeta = (TypeCheckerExpressionMeta, ParserNodeMeta)
type UntypedExpression = ExpMeta -> TypeCheckerExpression

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
  return $ Variable (id ++ show t) ()

call :: Bool -> Type -> Gen UntypedExpression
call pure _ = do
  name <- identifier
  args <- scale pred $ Sara.AstTestUtils.args pure
  return $ Call name args ()

args :: Bool -> Gen [TypeCheckerExpression]
args pure = scale intRoot $ listOf arg
  where arg = Sara.AstTestUtils.typ >>= expression pure

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
          where var = addExpMeta r (variable r)
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
  where stmts = scale intRoot $ listOf $ Sara.AstTestUtils.typ >>= expression pure
        exp = scale intRoot $ expression pure t

while :: Gen UntypedExpression
while = liftM2 While (subtree T.Boolean) (Sara.AstTestUtils.typ >>= subtree)
  where subtree t = scale (`div` 2) $ expression False t

leafExpression :: Type -> Gen UntypedExpression
leafExpression t = oneof [constant t, variable t]
  where constant T.Boolean = boolean
        constant T.Integer = integer
        constant T.Double  = double
        constant T.Unit    = return S.Unit

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

expression :: Bool -> Type -> Gen TypeCheckerExpression
expression pure t = addExpMeta t $ sized expression'
  where expression' :: Int -> Gen UntypedExpression
        expression' 0         = leafExpression t
        expression' n | n > 0 = innerExpression pure t
        expression' s         = error $ "expression' for negative size " ++ show s ++ " not supported."

arbitraryTypedVariable :: Type -> Gen ParserTypedVariable
arbitraryTypedVariable t = do
  name <- identifier
  return $ TypedVariable name t ((), mkNodeMeta)

arbitraryExtern :: FunctionType -> Gen TypeCheckerDeclaration
arbitraryExtern (FunctionType pure name argTypes retType) = do
  args <- mapM arbitraryTypedVariable argTypes
  return $ Extern (Signature pure name args retType [] [] ((), mkNodeMeta)) mkNodeMeta

fixName :: [Name] -> Name -> Name
fixName names name | name `elem` names = fixName names (name ++ "0")
                   | otherwise         = name

mkNodeMeta :: ParserNodeMeta
mkNodeMeta = ParserNodeMeta pos

arbitraryProgram :: Gen TypeCheckerProgram
arbitraryProgram = do
  decls <- listOf arbitrary
  completeProgram decls

completeProgram :: [TypeCheckerDeclaration] -> Gen TypeCheckerProgram
completeProgram decls = do
  let prog = Program decls mkNodeMeta
  let prog' = fixFunctionNameClashes prog
  let free = calledFunctions prog'
  externs <- mapM arbitraryExtern free
  return $ prog' { program = program prog' ++ externs }
    where fixFunctionNameClashes :: TypeCheckerProgram -> TypeCheckerProgram
          fixFunctionNameClashes prog = evalState (fixFunctionNameClashes' prog) []
          fixFunctionNameClashes' :: TypeCheckerProgram -> State [Name] TypeCheckerProgram
          fixFunctionNameClashes' prog =
            transformProgram id id return fixExpression fixSignature return prog
          fixSignature :: TypeCheckerSignature -> State [Name] TypeCheckerSignature
          fixSignature s@Signature{ S.sigName = n } = do
            n' <- fixName' n
            return $ s{ S.sigName = n' }
          fixExpression :: TypeCheckerExpression -> State [Name] TypeCheckerExpression
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

trivial :: ExpMeta -> TypeCheckerExpression
trivial m = trivial' (expTyp $ fst m) m
  where trivial' T.Boolean = S.Boolean False
        trivial' T.Integer = S.Integer 0
        trivial' T.Double  = S.Double 0.0
        trivial' T.Unit    = S.Unit

instance Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators

shrinkExpression :: TypeCheckerExpression -> [TypeCheckerExpression]
shrinkExpression b@S.Boolean{ boolValue = val }     = [b{ boolValue = v } | v <- shrink val]
shrinkExpression n@S.Integer{ intValue = val }      = [n{ intValue = v } | v <- shrink val]
shrinkExpression d@S.Double{ doubleValue = val }    = [d{ doubleValue = v } | v <- shrink val]
shrinkExpression S.Unit{}                           = []
shrinkExpression (Variable _ _ m)                   = [trivial m]
shrinkExpression (BinaryOperation op left right m)  = childrenWithType m [left, right]
                                                      ++ [BinaryOperation op l r m | (l, r) <- shrink (left, right)]
shrinkExpression (UnaryOperation op exp m)          = childrenWithType m [exp]
                                                      ++ [UnaryOperation op e m | e <- shrink exp]
shrinkExpression (Call name args cm m)              = childrenWithType m args
                                                      ++ [Call name a cm m | a <- shrinkArgs args]
  where shrinkArgs :: [TypeCheckerExpression] -> [[TypeCheckerExpression]]
        shrinkArgs []     = []
        shrinkArgs (x:xs) = [y : xs | y <- shrink x] ++ [x : ys | ys <- shrinkArgs xs]
shrinkExpression (Conditional cond ifExp elseExp m) = childrenWithType m [cond, ifExp, elseExp]
                                                      ++ [Conditional c i e m | (c, i, e) <- shrink (cond, ifExp, elseExp)]
shrinkExpression (Block stmts exp m)                = [exp]
                                                      ++ childrenWithType m stmts
                                                      ++ [Block (init stmts) (last stmts) m | not (null stmts), expressionTyp (last stmts) == expTyp (fst m)]
                                                      ++ [Block s e m | (s, e) <- shrink (stmts, exp)]
shrinkExpression (While cond body m)                = While cond (S.Unit (TypeCheckerExpressionMeta T.Unit, mkNodeMeta)) m
                                                      : [While c b m | (c, b) <- shrink (cond, body)]

childrenWithType :: ExpMeta -> [TypeCheckerExpression] -> [TypeCheckerExpression]
childrenWithType m = filter (\c -> expressionTyp c == (expTyp $ fst m))

shrinkTypedVariable :: ParserTypedVariable -> [ParserTypedVariable]
shrinkTypedVariable (TypedVariable var typ p) = [TypedVariable v typ p | v <- shrinkIdentifier var]

shrinkSignature :: [ParserTypedVariable] -> TypeCheckerSignature -> [TypeCheckerSignature]
shrinkSignature free (Signature pure name args typ precs posts p) =
  [Signature pure name args typ precs' posts' p | (precs', posts') <- shrink (precs, posts)]
  ++ [Signature pure name a typ precs posts p | a <- shrinkArgTypes args]
  where shrinkArgTypes :: [ParserTypedVariable] -> [[ParserTypedVariable]]
        shrinkArgTypes []                     = []
        shrinkArgTypes (x:xs) | x `elem` free = [x : ys | ys <- shrinkArgTypes xs]
                              | otherwise     = xs : [y : xs | y <- shrinkTypedVariable x]
                                                ++ [x : ys | ys <- shrinkArgTypes xs]

shrinkProgram :: TypeCheckerProgram -> [TypeCheckerProgram]
shrinkProgram p = shrinkProgram' (calledFunctions p) p
  where meta = S.progMeta p
        shrinkProgram' _ (Program [] _)         = []
        shrinkProgram' funcs (Program (x:xs) _) = headRemovals ++ map appendTail headShrinks ++ map appendHead tailShrinks
          where headShrinks = shrink x
                headRemovals = if isRemovable x then Program xs meta : [Program (d : xs) meta | d <- shrinkSig x] else []
                shrinkSig (Function sig body p) = [Function s body p | s <- shrinkSignature (freeVariables body) sig]
                shrinkSig (Extern sig p)          = [Extern s p | s <- shrinkSignature [] sig]
                isRemovable d = isRemovableSignature $ signature d
                isRemovableSignature Signature{ S.isPure = pure, sigName = name, S.args = args, retType = retType } =
                  FunctionType pure name (map varType args) retType `notElem` funcs
                appendTail y = Program (y:xs) meta
                tailShrinks = shrinkProgram' funcs $ Program xs meta
                appendHead (Program ys p) = Program (x : ys) p

newtype PureExpression
  = PureExpression { runPureExpression :: TypeCheckerExpression }
  deriving (Eq, Ord, Show)

instance Arbitrary TypeCheckerExpression where
  arbitrary = Sara.AstTestUtils.typ >>= expression False
  shrink = shrinkExpression

instance Arbitrary PureExpression where
  arbitrary = Sara.AstTestUtils.typ >>= liftM PureExpression . expression True
  shrink = map PureExpression . shrinkExpression . runPureExpression

instance Arbitrary TypeCheckerDeclaration where
  arbitrary = declaration
  shrink (Function sig body p) = [Function sig b p | b <- shrink body]
  shrink _                     = []

instance Arbitrary TypeCheckerProgram where
  arbitrary = arbitraryProgram
  shrink = shrinkProgram

instance Arbitrary Type where
  arbitrary = Sara.AstTestUtils.typ

clearPositions :: ParserProgram -> ParserProgram
clearPositions = mapNodeMetas $ const $ mkNodeMeta

clearTypes :: TypeCheckerProgram -> ParserProgram
clearTypes = mapExpressionMetas $ const ()
