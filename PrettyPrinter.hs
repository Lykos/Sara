module PrettyPrinter (
  pretty
  , prettyRender) where

import Text.PrettyPrint
import Types
import Syntax

prettyRender :: [DeclarationOrExpression] -> String
prettyRender = render . pretty

pretty :: [DeclarationOrExpression] -> Doc
pretty = vsep . punctuate semi . map prettyDeclarationOrExpression

indentation :: Int
indentation = 2

vsep :: [Doc] -> Doc
vsep = foldl ($+$) empty

prettyDeclarationOrExpression :: DeclarationOrExpression -> Doc
prettyDeclarationOrExpression (Left d)  = prettyDeclarationAst d
prettyDeclarationOrExpression (Right e) = prettyExpressionAst e

prettyDeclarationAst :: DeclarationAst -> Doc
prettyDeclarationAst = prettyDeclaration . decl

prettyDeclaration :: Declaration -> Doc
prettyDeclaration (Function sig body) = prettySignature sig
                                        $+$ nest indentation (prettyExpressionAst body)
prettyDeclaration (Extern sig)        = prettySignature sig

prettySignature :: Signature -> Doc
prettySignature (Signature name args retType) = prettyTyped sig retType
  where sig = text "function"
              <+> text name
              <> (parens . hsep . punctuate comma . map prettyTypedVariable) args

prettyTyped :: Doc -> Type -> Doc
prettyTyped doc Unknown = doc
prettyTyped doc typ     = doc
                          <> colon
                          <+> prettyType typ

prettyTypedVariable :: TypedVariable -> Doc
prettyTypedVariable (TypedVariable name typ) = prettyTyped (text name) typ

prettyType :: Type -> Doc
prettyType = text . show

prettyExpressionAst :: ExpressionAst -> Doc
prettyExpressionAst (ExpressionAst exp typ _) = case typ of
  Unknown -> prettyExpression exp
  _       -> prettyTyped (prettyTypeTerm exp) typ

prettyTermAst :: ExpressionAst -> Doc
prettyTermAst (ExpressionAst exp typ _) = prettyTyped (prettyTerm exp) typ

prettyTerm :: Expression -> Doc
prettyTerm exp = case exp of
  (BinaryOperation _ _ _) -> parens $ prettyExpression exp
  _                       -> prettyExpression exp

prettyTypeTerm :: Expression -> Doc
prettyTypeTerm exp = case exp of
  (UnaryOperation _ _) -> parens $ prettyExpression exp
  _                    -> prettyTerm exp

prettyExpression :: Expression -> Doc
prettyExpression (Syntax.Boolean True)            = text "true"
prettyExpression (Syntax.Boolean False)           = text "false"
prettyExpression (Syntax.Integer n)               = integer n
prettyExpression (Syntax.Double d)                = double d
prettyExpression (UnaryOperation op exp)          = (text . unarySymbol $ op)
                                                    <> prettyTermAst exp
prettyExpression (BinaryOperation op left right)  = prettyTermAst left
                                                    <+> (text . binarySymbol $ op)
                                                    <+> prettyTermAst right
prettyExpression (Variable var)                   = text var
prettyExpression (Call name args)                 = text name
                                                    <> (parens . hsep . punctuate comma . map prettyExpressionAst) args
prettyExpression (Conditional cond ifExp elseExp) = text "if"
                                                    <+> prettyExpressionAst cond
                                                    <+> text "then"
                                                    <+> prettyExpressionAst ifExp
                                                    <+> text "else"
                                                    <+> prettyExpressionAst elseExp

prettySubExp :: ExpressionAst -> Doc
prettySubExp ast = case astExp ast of
  b@(BinaryOperation _ _ _) -> parens . prettyExpressionAst $ ast
  _                         -> prettyExpressionAst ast
