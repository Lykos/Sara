module TestUtils where

import Syntax
import Types
import Lexer

import Control.Monad
import Test.QuickCheck
import Text.Parsec.Pos

iden0' :: [Char]
iden0' = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

iden0 :: Gen Char
iden0 = elements iden0'

idenN' :: [Char]
idenN' = iden0' ++ ['0'..'9']

idenN :: Gen [Char]
idenN = listOf $ elements idenN'

ident :: Gen String
ident = do
  i0 <- iden0
  iN <- idenN
  return (i0:iN)

identifier :: Gen String
identifier = ident `suchThat` isNotReserved

isNotReserved :: String -> Bool
isNotReserved a = a `notElem` reservedNames

shrinkIdentifier :: String -> [String]
shrinkIdentifier = filter (\a -> not (null a) && isNotReserved a) . shrink

shrinkWithIdentifier :: Arbitrary a => String -> a -> [(String, a)]
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
declaration = oneof [function, extern]

function :: Gen Declaration
function = liftM2 Function TestUtils.signature expressionAst

extern :: Gen Declaration
extern = liftM Extern TestUtils.signature

signature :: Gen Signature
signature = liftM3 Signature identifier (listOf typedVariable) typ

typedVariable :: Gen TypedVariable
typedVariable = liftM2 TypedVariable identifier typ

expressionAst :: Gen ExpressionAst
expressionAst = liftM3 ExpressionAst expression typ position

typ :: Gen Type
typ = elements [Types.Boolean, Types.Integer, Types.Double]

boolean :: Gen Expression
boolean = liftM Syntax.Boolean arbitrary

integer :: Gen Expression
integer = liftM (Syntax.Integer . abs) arbitrary

double :: Gen Expression
double = liftM Syntax.Double $ elements [0.0, 0.1, 1.0, 1.1, 1e10, 1.1e10]

variable :: Gen Expression
variable = liftM Variable identifier

call :: Gen Expression
call = sized call'
  where call' 0 = error "Tried to generate a call for size 0."
        call' n = liftM2 Call identifier (resize (n - 1) TestUtils.args)

args :: Gen [ExpressionAst]
args = sized args'
  where args' :: Int -> Gen [ExpressionAst]
        args' n   = resize (intRoot n) (listOf expressionAst)
        intRoot = round . sqrt . fromIntegral

binaryOperation :: Gen Expression
binaryOperation = sized binaryOperation'
  where binaryOperation' 0 = error "Tried to generate a binary operation for size 0."
        binaryOperation' n = liftM3 BinaryOperation arbitrary subtree subtree
          where subtree = resize (n `div` 2) expressionAst
                                 
unaryOperation :: Gen Expression
unaryOperation = sized unaryOperation'
  where unaryOperation' 0 =  error "Tried to generate a unary operation for size 0."
        unaryOperation' n = liftM2 UnaryOperation arbitrary (resize (n - 1) expressionAst)

conditional :: Gen Expression
conditional = sized conditional'
  where conditional' 0 =  error "Tried to generate a conditional for size 0."
        conditional' n = liftM3 Conditional subtree subtree subtree
          where subtree = resize (n `div` 3) expressionAst

expression :: Gen Expression
expression = sized expression'
    where leaves                = [boolean, integer, double, variable]
          innerNodes            = [unaryOperation, binaryOperation, conditional, call]
          expression' 0         = oneof leaves
          expression' n | n > 0 = oneof $ leaves ++ innerNodes

instance Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators

instance Arbitrary TypedVariable where
  arbitrary = typedVariable
  shrink (TypedVariable name typ) = [TypedVariable n typ | n <- shrinkIdentifier name]

instance Arbitrary Signature where
  arbitrary = TestUtils.signature
  shrink (Signature name args typ) = [Signature n a typ | (n, a) <- shrinkWithIdentifier name args]

instance Arbitrary ExpressionAst where
  arbitrary = expressionAst
  shrink (ExpressionAst exp typ pos) = [ExpressionAst e typ pos | e <- shrink exp]

instance Arbitrary Expression where
  arbitrary = expression
  shrink (Syntax.Boolean b)               = map Syntax.Boolean $ shrink b
  shrink (Syntax.Integer n)               = map Syntax.Integer $ shrink n
  shrink (Syntax.Double d)                = map Syntax.Double $ shrink d
  shrink (Variable v)                     = map Variable $ shrinkIdentifier v
  shrink (BinaryOperation op left right)  = map astExp [left, right] ++ [BinaryOperation op l r | (l, r) <- shrink (left, right)]
  shrink (UnaryOperation op exp)          = map astExp [exp] ++ [UnaryOperation op e | e <- shrink exp]
  shrink (Conditional cond ifExp elseExp) = map astExp [cond, ifExp, elseExp] ++ [Conditional c i e | (c, i, e) <- shrink (cond, ifExp, elseExp)]
  shrink (Call name args)                 = (args >>= map astExp . shrink) ++ [Call n a | (n, a) <- shrinkWithIdentifier name args]

instance Arbitrary DeclarationAst where
  arbitrary = declarationAst
  shrink (DeclarationAst decl pos) = [DeclarationAst d pos | d <- shrink decl]

instance Arbitrary Declaration where
  arbitrary = declaration
  shrink (Function sig body) = [Function s b | (s, b) <- shrink (sig, body)]
  shrink _                   = []

clearPositions :: [DeclarationOrExpression] -> [DeclarationOrExpression]
clearPositions = mapDeclarationAst clearPosDeclarationAst . mapExpressionAst clearPosExpressionAst
  where clearPosDeclarationAst (DeclarationAst decl _) = DeclarationAst decl pos
        clearPosExpressionAst (ExpressionAst exp typ _) = ExpressionAst exp typ pos

clearTypes :: [DeclarationOrExpression] -> [DeclarationOrExpression]
clearTypes = mapExpressionAst clearTypesExpressionAst
  where clearTypesExpressionAst (ExpressionAst exp _ pos) = ExpressionAst exp Unknown pos
