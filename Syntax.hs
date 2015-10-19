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
  | Method Signature ExpressionAst
  deriving (Eq, Ord, Show)

signature :: Declaration -> Signature
signature (Function sig _) = sig
signature (Extern sig)     = sig
signature (Method sig _)   = sig

data Signature
  = Signature { funcName :: Name
              , args :: [TypedVariable]
              , returnType :: Type }
  deriving (Eq, Ord, Show)

data TypedVariable
  = TypedVariable { varName :: Name
                  , varType :: Type
                  , varPos :: SourcePos }
  deriving (Eq, Ord, Show)

-- Expression AST node that contains the expression and some metadata.
data ExpressionAst
  = ExpressionAst { astExp :: Expression
                  , expType :: Type
                  , expPos :: SourcePos }
  deriving (Eq, Ord, Show)

data Expression
  = Unit
  | Boolean Bool
  | Integer Integer
  | Double Double
  | UnaryOperation UnaryOperator ExpressionAst
  | BinaryOperation BinaryOperator ExpressionAst ExpressionAst
  | Variable Name
  | Call Name [ExpressionAst]
  | Conditional ExpressionAst ExpressionAst ExpressionAst
  | Block [ExpressionAst] ExpressionAst
  | While ExpressionAst ExpressionAst
  deriving (Eq, Ord, Show)

newtype Program
  = Program { program :: [DeclarationAst] }
  deriving (Eq, Ord, Show)
