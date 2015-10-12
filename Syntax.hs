module Syntax where

import Data.Bifunctor
import Types
import Text.Parsec.Pos
import Operators

type Name = String

-- Declaration AST node that contains the declaration and some metadata.
data DeclarationAst
  = DeclarationAst { decl :: Declaration
                   , declPos :: SourcePos }
  deriving (Eq, Ord, Show)

data Declaration
  = Function Signature ExpressionAst
  | Extern Signature
  deriving (Eq, Ord, Show)

signature :: Declaration -> Signature
signature (Function sig _) = sig
signature (Extern sig)     = sig

data Signature
  = Signature { funcName :: Name
              , args :: [TypedVariable]
              , returnType :: Type }
  deriving (Eq, Ord, Show)

data TypedVariable
  = TypedVariable { varName :: Name
                  , varType :: Type }
  deriving (Eq, Ord, Show)

-- Expression AST node that contains the expression and some metadata.
data ExpressionAst
  = ExpressionAst { astExp :: Expression
                  , expType :: Type
                  , expPos :: SourcePos }
  deriving (Eq, Ord, Show)

mapExpressionAstExpressionAst :: (ExpressionAst -> ExpressionAst) -> ExpressionAst -> ExpressionAst
mapExpressionAstExpressionAst f (ExpressionAst exp typ pos) = f (ExpressionAst mappedExp typ pos)
    where mapSubExp = mapExpressionAstExpressionAst f
          mappedExp = case exp of
            BinaryOperation op left right  -> BinaryOperation op (mapSubExp left) (mapSubExp right)
            UnaryOperation op exp          -> UnaryOperation op (mapSubExp exp)
            Conditional cond ifExp elseExp -> Conditional (mapSubExp cond) (mapSubExp ifExp) (mapSubExp elseExp)
            Call name args                 -> Call name (map (mapSubExp) args)
            e                              -> e

mapExpressionAst :: (ExpressionAst -> ExpressionAst) -> Program -> Program
mapExpressionAst f (Program p) = Program $ map (second (mapExpressionAstExpressionAst f) . first mapExpressionAstFunction) p
  where mapExpressionAstFunction (DeclarationAst (Function sig body) pos) = DeclarationAst (Function sig (mapExpressionAstExpressionAst f body)) pos
        mapExpressionAstFunction d                                        = d

mapDeclarationAst :: (DeclarationAst -> DeclarationAst) -> Program -> Program
mapDeclarationAst f = (Program . ((map . first) f)) . program

data Expression
  = Boolean Bool
  | Integer Integer
  | Double Double
  | UnaryOperation UnaryOperator ExpressionAst
  | BinaryOperation BinaryOperator ExpressionAst ExpressionAst
  | Variable Name
  | Call Name [ExpressionAst]
  | Conditional ExpressionAst ExpressionAst ExpressionAst
  deriving (Eq, Ord, Show)

type DeclarationOrExpression = Either DeclarationAst ExpressionAst

newtype Program
  = Program { program :: [DeclarationOrExpression] }
  deriving (Eq, Ord, Show)
