{-# LANGUAGE TemplateHaskell #-}

module ParserTest (parserCheck) where

import TestUtils
import Parser
import PrettyPrinter
import Syntax

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property
import Text.Parsec

clearPosDeclarationOrExpression :: DeclarationOrExpression -> DeclarationOrExpression
clearPosDeclarationOrExpression (Left e)  = Left $ clearPosDeclarationAst e
clearPosDeclarationOrExpression (Right e) = Right $ clearPosExpressionAst e

clearPosExpressionAst :: ExpressionAst -> ExpressionAst
clearPosExpressionAst (ExpressionAst exp typ _) = ExpressionAst (clearPosExpression exp) typ pos

clearPosExpression :: Expression -> Expression
clearPosExpression (BinaryOperation op left right)  = BinaryOperation op (clearPosExpressionAst left) (clearPosExpressionAst right)
clearPosExpression (UnaryOperation op exp)          = UnaryOperation op (clearPosExpressionAst exp)
clearPosExpression (Conditional cond ifExp elseExp) = Conditional (clearPosExpressionAst cond) (clearPosExpressionAst ifExp) (clearPosExpressionAst elseExp)
clearPosExpression (Call name args)                 = Call name (map clearPosExpressionAst args)
clearPosExpression e                                = e

clearPosDeclarationAst :: DeclarationAst -> DeclarationAst
clearPosDeclarationAst (DeclarationAst decl _) = DeclarationAst (clearPosDeclaration decl) pos

clearPosDeclaration :: Declaration -> Declaration
clearPosDeclaration (Function sig body) = Function sig $ clearPosExpressionAst body
clearPosDeclaration decl                = decl

clearPosDeclarationsOrExpressions :: [DeclarationOrExpression] -> [DeclarationOrExpression]
clearPosDeclarationsOrExpressions = map clearPosDeclarationOrExpression

clearPos :: Either ParseError [DeclarationOrExpression] -> Either ParseError [DeclarationOrExpression]
clearPos (Right s) = Right $ clearPosDeclarationsOrExpressions s
clearPos e         = e

prop_prettyInv :: [DeclarationOrExpression] -> Property
prop_prettyInv xs = example `counterexample` liftBool (actual == expected)
  where example = "\nExpected:\n" ++ show expected
                  ++ "\nActual:\n" ++ show actual
                  ++ "\nInput:\n" ++ code
        expected :: Either ParseError [DeclarationOrExpression]
        expected = Right xs
        code = prettyRender xs
        actual = clearPos (Parser.parse testfile code)

parserCheck = $quickCheckAll
