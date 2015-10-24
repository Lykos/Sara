module PrettyPrinter (
  pretty
  , prettyRender) where

import Text.PrettyPrint
import Types
import AstUtils
import Operators
import Syntax

prettyRender :: Program -> String
prettyRender = render . pretty

pretty :: Program -> Doc
pretty = vsep . punctuate semi . map prettyDeclaration . program

indentation :: Int
indentation = 2

vsep :: [Doc] -> Doc
vsep = foldl ($+$) empty

prettyDeclaration :: Declaration -> Doc
prettyDeclaration (Function sig body _) = prettyFunctionOrMethod "function" sig body
prettyDeclaration (Method sig body _)   = prettyFunctionOrMethod "method" sig body
prettyDeclaration (Extern sig _)        = text "extern" <+> prettySignature sig

prettyFunctionOrMethod :: String -> Signature -> Expression -> Doc
prettyFunctionOrMethod keyword sig body = case body of
  Block{} -> sigDoc <+> prettyExpression body
  _       -> sigDoc $+$ nest indentation (prettyExpression body)
  where sigDoc = text keyword <+> prettySignature sig <+> text "="

prettySignature :: Signature -> Doc
prettySignature (Signature name args retType _) = prettyTyped sig retType
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

prettyExpression :: Expression -> Doc
prettyExpression exp = prettyTyped (prettyUntypedExpression exp) (typ exp)

prettyBinaryTerm :: Expression -> Doc
prettyBinaryTerm exp = prettyTyped (prettyUntypedBinaryTerm exp) (typ exp)

prettyTerm :: Expression -> Doc
prettyTerm exp = prettyTyped (prettyUntypedTerm exp) (typ exp)

prettyUntypedBinaryTerm :: Expression -> Doc
prettyUntypedBinaryTerm exp = case exp of
  BinaryOperation{} -> parens $ prettyUntypedExpression exp
  Conditional{}     -> parens $ prettyUntypedExpression exp
  _                 -> prettyUntypedExpression exp

prettyUntypedTerm :: Expression -> Doc
prettyUntypedTerm exp = case exp of
  UnaryOperation{} -> parens $ prettyExpression exp
  _                -> prettyBinaryTerm exp

prettyUntypedExpression :: Expression -> Doc
prettyUntypedExpression Syntax.Unit{}                        = text "()"
prettyUntypedExpression (Syntax.Boolean True _ _)            = text "true"
prettyUntypedExpression (Syntax.Boolean False _ _)           = text "false"
prettyUntypedExpression (Syntax.Integer n _ _)               = integer n
prettyUntypedExpression (Syntax.Double d _ _)                = double d
prettyUntypedExpression (UnaryOperation op exp _ _)          = (text . unarySymbol $ op)
                                                               <> prettyTerm exp
prettyUntypedExpression (BinaryOperation op left right _ _)  = prettyBinaryTerm left
                                                               <+> (text . binarySymbol $ op)
                                                               <+> prettyBinaryTerm right
prettyUntypedExpression (Variable var _ _)                   = text var
prettyUntypedExpression (Call name args _ _)                 = text name
                                                               <> (parens . hsep . punctuate comma . map prettyExpression) args
prettyUntypedExpression (Conditional cond ifExp elseExp _ _) = text "if"
                                                               <+> prettyExpression cond
                                                               <+> text "then"
                                                               <+> prettyExpression ifExp
                                                               <+> text "else"
                                                               <+> prettyExpression elseExp
prettyUntypedExpression (Block [] (Syntax.Unit _ _) _ _)     = text "{}"  -- This is necessary to make the pretty printer the inverse of the parser.
prettyUntypedExpression (Block stmts exp _ _)                = inBlock (vsep . punctuate semi . map prettyExpression $ stmts ++ [exp])
prettyUntypedExpression (While cond body _ _)                = text "while" <+> prettyExpression cond <+> inBlock (prettyExpression body)

inBlock :: Doc -> Doc
inBlock doc = text "{" $+$ nest indentation doc $+$ text "}"
