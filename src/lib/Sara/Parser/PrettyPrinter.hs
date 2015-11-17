{-# LANGUAGE RecordWildCards #-}

module Sara.Parser.PrettyPrinter (
  Pretty
  , pretty
  , prettyRender) where

import qualified Sara.Parser.Keywords as K
import Text.PrettyPrint
import Sara.Ast.Types
import Sara.Ast.Operators
import Sara.Ast.Syntax as S

prettyRender :: Pretty a => a -> String
prettyRender = render . pretty

class Pretty a where
  pretty :: a -> Doc

instance Pretty (Program a b c d) where
  pretty = prettyProgram

instance Pretty (Signature a b c d) where
  pretty = prettySignature

instance Pretty (TypedVariable b d) where
  pretty = prettyTypedVariable

instance Pretty Type where
  pretty = prettyType

instance Pretty (Expression a b c d) where
  pretty = prettyExpression

prettyProgram :: Program a b c d -> Doc
prettyProgram = vsep . punctuate semi . map prettyDeclaration . program

indentation :: Int
indentation = 2

vsep :: [Doc] -> Doc
vsep = foldl ($+$) empty

prettyDeclaration :: Declaration a b c d -> Doc
prettyDeclaration (Function sig body _) = prettyFunction sig body
prettyDeclaration (Extern sig _)        = keyword K.Extern <+> prettySignature sig

prettyFunction :: Signature a b c d -> Expression a b c d -> Doc
prettyFunction sig body = prettySignature sig $+$ inBlock (prettyExpression body)

prettySignature :: Signature a b c d -> Doc
prettySignature Signature{..} = prettyTyped sig retType
                                $+$ conditions K.Requires preconditions
                                $+$ conditions K.Ensures postconditions
  where sig = keyword word <+> text sigName
              <> (parens . hsep . punctuate comma . map prettyTypedVariable) args
        word = if isPure then K.Function else K.Method

conditions :: K.Keyword -> [Expression a b c d] -> Doc
conditions word conds = nest (2 * indentation) $ vsep $ punctuate semi $ map toCond $ conds
  where toCond cond = keyword word <+> prettyExpression cond

prettyTyped :: Doc -> Type -> Doc
prettyTyped doc typ     = doc
                          <> colon
                          <+> prettyType typ

prettyTypedVariable :: TypedVariable b d -> Doc
prettyTypedVariable TypedVariable{..} = prettyTyped (text varName) varType

prettyType :: Type -> Doc
prettyType = text . show

-- TODO types
prettyExpression :: Expression a b c d -> Doc
prettyExpression exp = prettyUntypedExpression exp

prettyBinaryTerm :: Expression a b c d -> Doc
prettyBinaryTerm exp = prettyUntypedBinaryTerm exp

prettyTerm :: Expression a b c d -> Doc
prettyTerm exp = prettyUntypedTerm exp

prettyUntypedBinaryTerm :: Expression a b c d -> Doc
prettyUntypedBinaryTerm exp = case exp of
  BinaryOperation{} -> parens $ prettyUntypedExpression exp
  Conditional{}     -> parens $ prettyUntypedExpression exp
  _                 -> prettyUntypedExpression exp

prettyUntypedTerm :: Expression a b c d -> Doc
prettyUntypedTerm exp = case exp of
  UnaryOperation{} -> parens $ prettyExpression exp
  _                -> prettyBinaryTerm exp

prettyUntypedExpression :: Expression a b c d -> Doc
prettyUntypedExpression S.Unit{}                             = text "()"
prettyUntypedExpression (S.Boolean True _ _)                 = keyword K.True
prettyUntypedExpression (S.Boolean False _ _)                = keyword K.False
prettyUntypedExpression (S.Integer n _ _)                    = integer n
prettyUntypedExpression (S.Double d _ _)                     = double d
prettyUntypedExpression (UnaryOperation op exp _ _)          = (text . unarySymbol $ op)
                                                             <> prettyTerm exp
prettyUntypedExpression (BinaryOperation op left right _ _)  = prettyBinaryTerm left
                                                             <+> (text . binarySymbol $ op)
                                                             <+> prettyBinaryTerm right
prettyUntypedExpression (Variable var _ _ _)                 = text var
prettyUntypedExpression (Call name args _ _ _)               = text name
                                                             <> (parens . hsep . punctuate comma . map prettyExpression) args
prettyUntypedExpression (Conditional cond ifExp elseExp _ _) = keyword K.If
                                                             <+> prettyExpression cond
                                                             <+> keyword K.Then
                                                             <+> prettyExpression ifExp
                                                             <+> keyword K.Else
                                                             <+> prettyExpression elseExp
prettyUntypedExpression (Block [] S.Unit{} _ _)              = text "{}"  -- This special case is necessary to make the pretty printer the inverse of the parser.
prettyUntypedExpression (Block stmts exp _ _)                = inBlock (vsep . punctuate semi . map prettyExpression $ stmts ++ [exp])
prettyUntypedExpression (While invs cond body _ _)           = keyword K.While <+> parens (prettyExpression cond)
                                                             $+$ conditions K.Invariant invs
                                                             $+$ inBlock (prettyExpression body)
prettyUntypedExpression (Assertion kind exp _ _)             = assertKeyword kind <+> prettyExpression exp
prettyUntypedExpression (VarDef tVar isVal exp _ _)          = keyword word <+> prettyTypedVariable tVar <+> equals <+> prettyExpression exp
  where word = if isVal then K.Val else K.Var

assertKeyword :: AssertionKind -> Doc
assertKeyword Assert            = keyword K.Assert
assertKeyword Assume            = keyword K.Assume
assertKeyword AssertAndCollapse = keyword K.AssertAndCollapse

inBlock :: Doc -> Doc
inBlock doc = text "{" $+$ nest indentation doc $+$ text "}"

keyword :: K.Keyword -> Doc
keyword = text . K.keyword
