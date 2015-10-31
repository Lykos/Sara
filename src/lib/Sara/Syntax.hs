module Sara.Syntax where

import Sara.Types
import Text.Parsec.Pos
import Sara.Operators

type Name = String

class Positioned p where
  position :: p -> SourcePos

class Named n where
  name :: n -> Name

class Typed t where
  typ :: t -> Type

data Declaration
  = Function { signature :: Signature, body :: Expression, declPos :: SourcePos }
  | Extern { signature :: Signature, declPos :: SourcePos }
  deriving (Eq, Ord, Show)

instance Positioned Declaration where
  position = declPos

data Signature
  = Signature { pure :: Bool
              , sigName :: Name
              , args :: [TypedVariable]
              , retType :: Type
              , preconditions :: [Expression]
              , postconditions :: [Expression]
              , sigPos :: SourcePos }
  deriving (Eq, Ord, Show)

instance Positioned Signature where
  position = sigPos

instance Named Signature where
  name = sigName

instance Typed Signature where
  typ = retType

data TypedVariable
  = TypedVariable { varName :: Name
                  , varType :: Type
                  , varPos :: SourcePos }
  deriving (Eq, Ord, Show)

instance Positioned TypedVariable where
  position = varPos

instance Named TypedVariable where
  name = varName

instance Typed TypedVariable where
  typ = varType

data Expression
  = Unit { expType :: Type, expPos :: SourcePos }
  | Boolean { boolValue :: Bool, expType :: Type, expPos :: SourcePos }
  | Integer { intValue :: Integer, expType :: Type, expPos :: SourcePos }
  | Double { doubleValue :: Double, expType :: Type, expPos :: SourcePos }
  | UnaryOperation { unOp :: UnaryOperator, inner :: Expression, expType :: Type, expPos :: SourcePos }
  | BinaryOperation { binOp :: BinaryOperator, left :: Expression, right :: Expression, expType :: Type, expPos :: SourcePos }
  | Variable { expName :: Name, expType :: Type, expPos :: SourcePos }
  | Call { expName :: Name, expArgs :: [Expression], expType :: Type, expPos :: SourcePos }
  | Conditional { cond :: Expression, thenExp :: Expression, elseExp :: Expression, expType :: Type, expPos :: SourcePos }
  | Block { stmts :: [Expression], inner :: Expression, expType :: Type, expPos :: SourcePos }
  | While { cond :: Expression, inner :: Expression, expType :: Type, expPos :: SourcePos }
  deriving (Eq, Ord, Show)

instance Typed Expression where
  typ = expType

instance Positioned Expression where
  position = expPos

data Program
  = Program { program :: [Declaration], progPos :: SourcePos }
  deriving (Eq, Ord, Show)

instance Positioned Program where
  position = progPos
