module PrettyPrinter (
  pretty
  , prettyRender) where

import Text.PrettyPrint
import Types
import Operators
import Syntax

prettyRender :: Program -> String
prettyRender = render . pretty

pretty :: Program -> Doc
pretty = vsep . punctuate semi . map prettyDeclarationAst . program

indentation :: Int
indentation = 2

vsep :: [Doc] -> Doc
vsep = foldl ($+$) empty

prettyDeclarationAst :: DeclarationAst -> Doc
prettyDeclarationAst = prettyDeclaration . decl

prettyDeclaration :: Declaration -> Doc
prettyDeclaration (Function sig body) = text "function" <+> prettySignature sig <+> text "="
                                        $+$ nest indentation (prettyExpressionAst body)
prettyDeclaration (Extern sig)        = text "extern" <+> prettySignature sig

prettySignature :: Signature -> Doc
prettySignature (Signature name args retType) = prettyTyped sig retType
  where sig = text name
              <> (parens . hsep . punctuate comma . map prettyTypedVariable) args

prettyTyped :: Doc -> Type -> Doc
prettyTyped doc Unknown = doc
prettyTyped doc typ     = doc
                          <> colon
                          <+> prettyType typ

prettyTypedVariable :: TypedVariable -> Doc
prettyTypedVariable (TypedVariable name typ _) = prettyTyped (text name) typ

prettyType :: Type -> Doc
prettyType = text . show

prettyExpressionAst :: ExpressionAst -> Doc
prettyExpressionAst (ExpressionAst exp typ _) = case typ of
  Unknown -> prettyExpression exp
  _       -> prettyTyped (prettyTerm exp) typ

prettyBinaryTermAst :: ExpressionAst -> Doc
prettyBinaryTermAst (ExpressionAst exp typ _) = prettyTyped (prettyBinaryTerm exp) typ

prettyTermAst :: ExpressionAst -> Doc
prettyTermAst (ExpressionAst exp typ _) = prettyTyped (prettyTerm exp) typ

prettyBinaryTerm :: Expression -> Doc
prettyBinaryTerm exp = case exp of
  (BinaryOperation _ _ _) -> parens $ prettyExpression exp
  (Conditional _ _ _)     -> parens $ prettyExpression exp
  _                       -> prettyExpression exp

prettyTerm :: Expression -> Doc
prettyTerm exp = case exp of
  (UnaryOperation _ _) -> parens $ prettyExpression exp
  _                    -> prettyBinaryTerm exp

prettyExpression :: Expression -> Doc
prettyExpression (Syntax.Boolean True)            = text "true"
prettyExpression (Syntax.Boolean False)           = text "false"
prettyExpression (Syntax.Integer n)               = integer n
prettyExpression (Syntax.Double d)                = double d
prettyExpression (UnaryOperation op exp)          = (text . unarySymbol $ op)
                                                    <> prettyTermAst exp
prettyExpression (BinaryOperation op left right)  = prettyBinaryTermAst left
                                                    <+> (text . binarySymbol $ op)
                                                    <+> prettyBinaryTermAst right
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
