module Sara.Z3.Pretty ( prettyRender ) where

import Text.PrettyPrint
import Sara.Z3.Ast
import Sara.Z3.Utils
import Sara.Meta

prettyRender :: Ast -> String
prettyRender = render . pretty

pretty :: Ast -> Doc
pretty (BoolConst True)   = text "true"
pretty (BoolConst False)  = text "false"
pretty (IntConst n)       = integer n
pretty (Var v)            = variable v
pretty (App a bs)         = function a <> (parens . hsep . punctuate comma . map pretty) bs
pretty (UnOp op a)        = parens $ unary op <> pretty a
pretty (BinOp op a b)     = parens $ pretty a <+> binary op <+> pretty b
pretty (NaryOp op [])     = pretty $ neutral op
pretty (NaryOp _ [a])     = pretty a
pretty (NaryOp op (a:as)) = parens $ sep $ pretty a : map ((nary op <+>) . pretty) as
pretty (Ite a b c)        = text "if" <+> pretty a <+> text "then" <+> pretty b <+> text "else" <+> pretty c
pretty (Forall as b cs)   = text "forall"
                           <+> (hsep . punctuate comma . map typedVariable) as
                           <+> dot
                           <+> pretty b
                           <+> colon <> text "pattern"
                           <+> hsep (map pattern cs)

-- TODO unify with codegen definition.
variable :: VariableMeta -> Doc
variable = text . z3VarName

-- TODO unify with codegen definition.
function :: AppMeta -> Doc
function = text . z3FuncName

typedVariable :: VariableMeta -> Doc
typedVariable m = variable m <> colon <+> text (show $ varSymType m)

unary :: UnaryOperator -> Doc
unary = text . unary'
  where unary' UnMinus = "-"
        unary' Not     = "!"

binary :: BinaryOperator -> Doc
binary = text . binary'
  where binary' DividedBy   = "/"
        binary' Modulo      = "%"
        binary' Minus       = "-"
        binary' LessThan    = "<"
        binary' AtMost      = "<="
        binary' GreaterThan = ">"
        binary' AtLeast     = ">="
        binary' EqualTo     = "=="
        binary' Xor         = "^"
        binary' Implies     = "==>"

nary :: NaryOperator -> Doc
nary = text . nary'
  where nary' Times = "*"
        nary' Plus  = "+"
        nary' And   = "&&"
        nary' Or    = "||"

pattern :: Pattern -> Doc
pattern (Pattern as) = parens $ sep $ map pretty as

dot :: Doc
dot = char '.'
