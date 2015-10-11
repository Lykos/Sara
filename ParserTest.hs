{-# LANGUAGE TemplateHaskell #-}

module ParserTest (parserCheck) where

import CommonArbitraryInstances
import Parser
import PrettyPrinter
import Syntax
import Types

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property
import Text.Parsec
import Text.Parsec.Pos

iden0' :: [Char]
iden0' = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

iden0 :: Gen Char
iden0 = elements iden0'

idenN' :: [Char]
idenN' = iden0' ++ ['0'..'9']

idenN :: Gen [Char]
idenN = listOf $ elements idenN'

identifier :: Gen String
identifier = do
  i0 <- iden0
  iN <- idenN
  return (i0:iN)

shrinkIdentifier :: String -> [String]
shrinkIdentifier = filter (\a -> not $ null a) . shrink

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
function = liftM2 Function ParserTest.signature expressionAst

extern :: Gen Declaration
extern = liftM Extern ParserTest.signature

signature :: Gen Signature
signature = liftM3 Signature identifier (listOf typedVariable) typ

typedVariable :: Gen TypedVariable
typedVariable = liftM2 TypedVariable identifier typ

expressionAst :: Gen ExpressionAst
expressionAst = do
    astExp <- expression
    expPos <- position
    return $ ExpressionAst astExp Unknown expPos

typ :: Gen Type
typ = elements [Types.Boolean, Types.Integer, Types.Double]

boolean :: Gen Expression
boolean = liftM Syntax.Boolean arbitrary

integer :: Gen Expression
integer = liftM Syntax.Integer arbitrary

double :: Gen Expression
double = liftM Syntax.Double arbitrary

variable :: Gen Expression
variable = liftM Variable identifier

call :: Gen Expression
call = sized call'
  where call' 0 = error "Tried to generate a call for size 0."
        call' n = liftM2 Call identifier (resize (n - 1) ParserTest.args)

args :: Gen [ExpressionAst]
args = sized args'
  where args' :: Int -> Gen [ExpressionAst]
        args' n   = resize (intRoot n) (listOf expressionAst)
        intRoot = round . sqrt . fromIntegral

binaryOperation :: Gen Expression
binaryOperation = sized binaryOperation'
  where binaryOperation' 0 =  error "Tried to generate a binary operation for size 0."
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

clearPosDeclarationOrExpression :: DeclarationOrExpression -> DeclarationOrExpression
clearPosDeclarationOrExpression (Left e)  = Left $ clearPosDeclaration e
clearPosDeclarationOrExpression (Right e) = Right $ clearPosExpressionAst e

clearPosExpressionAst :: ExpressionAst -> ExpressionAst
clearPosExpressionAst (ExpressionAst exp typ _) = ExpressionAst (clearPosExpression exp) typ pos

clearPosExpression :: Expression -> Expression
clearPosExpression (BinaryOperation op left right)  = BinaryOperation op (clearPosExpressionAst left) (clearPosExpressionAst right)
clearPosExpression (UnaryOperation op exp)          = UnaryOperation op (clearPosExpressionAst exp)
clearPosExpression (Conditional cond ifExp elseExp) = Conditional (clearPosExpressionAst cond) (clearPosExpressionAst ifExp) (clearPosExpressionAst elseExp)
clearPosExpression (Call name args)                 = Call name (map clearPosExpressionAst args)
clearPosExpression e                                = e

clearPosDeclaration :: DeclarationAst -> DeclarationAst
clearPosDeclaration (DeclarationAst decl _) = DeclarationAst decl pos

clearPosDeclarationsOrExpressions :: [DeclarationOrExpression] -> [DeclarationOrExpression]
clearPosDeclarationsOrExpressions = map clearPosDeclarationOrExpression

clearPos :: Either ParseError [DeclarationOrExpression] -> Either ParseError [DeclarationOrExpression]
clearPos (Right s) = Right $ clearPosDeclarationsOrExpressions s
clearPos e         = e

instance Arbitrary TypedVariable where
  arbitrary = typedVariable
  shrink (TypedVariable name typ) = [TypedVariable n typ | n <- shrinkIdentifier name]

instance Arbitrary Signature where
  arbitrary = ParserTest.signature
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

prop_prettyInv xs = code `counterexample` liftBool (clearPos (Parser.parse "<testinput>" code) == Right xs)
  where code = prettyRender xs

parserCheck = $quickCheckAll
