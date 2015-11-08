{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Sara.Syntax where

import Sara.Types
import Sara.Operators

type Name = String

class HasExpressionMeta a b where
  expressionMeta :: a -> b

class HasFunctionMeta a b where
  functionMeta :: a -> b

class HasVariableMeta a b where
  variableMeta :: a -> b

class HasNodeMeta a b where
  nodeMeta :: a -> b

-- | Declaration with 4 types of metadata:
-- [a:] The metadata used for signatures and calls.
-- [b:] The metadata used for variables and variable declarations.
-- [c:] The metadata used for all expressions.
-- [d:] The metadata used for all nodes.
data Declaration a b c d
  = Function { signature :: Signature a b c d, body :: Expression a b c d, declMeta :: d }
  | Extern { signature :: Signature a b c d, declMeta :: d }
  deriving (Eq, Ord, Show)

instance HasNodeMeta (Declaration a b c d) d where
  nodeMeta = declMeta

-- | Signature with 4 types of metadata:
-- [a:] The metadata used for signatures and calls.
-- [b:] The metadata used for variables and variable declarations.
-- [c:] The metadata used for all expressions.
-- [d:] The metadata used for all nodes.
data Signature a b c d
  = Signature { isPure :: Bool
              , sigName :: Name
              , args :: [TypedVariable b d]
              , retType :: Type
              , preconditions :: [Expression a b c d]
              , postconditions :: [Expression a b c d]
              , sigMeta :: (a, d) }
  deriving (Eq, Ord, Show)

instance HasNodeMeta (Signature a b c d) d where
  nodeMeta = snd . sigMeta

instance HasFunctionMeta (Signature a b c d) a where
  functionMeta = fst . sigMeta

-- | Typed variable with 2 types of metadata:
-- [b:] The metadata used for variables and variable declarations.
-- [d:] The metadata used for all nodes.
data TypedVariable b d
  = TypedVariable { varName :: Name
                  , varType :: Type
                  , varMeta :: (b, d) }
  deriving (Eq, Ord, Show)

instance HasVariableMeta (TypedVariable b d) b where
  variableMeta = fst . varMeta

instance HasNodeMeta (TypedVariable b d) d where
  nodeMeta = snd . varMeta

data AssertionKind
  = Assert
  | Assume
  | AssertAndCollapse
  deriving (Eq, Ord, Show, Enum, Bounded)

assertionKinds :: [AssertionKind]
assertionKinds = enumFrom minBound

-- | Expression with 4 types of metadata:
-- [a:] The metadata used for signatures and calls.
-- [b:] The metadata used for variables and variable declarations.
-- [c:] The metadata used for all expressions.
-- [d:] The metadata used for all nodes.
data Expression a b c d
  = Unit { expMeta :: (c, d) }
  | Assertion { assertionKind :: AssertionKind, inner :: Expression a b c d, expMeta :: (c, d) }
  | Boolean { boolValue :: Bool, expMeta :: (c, d) }
  | Integer { intValue :: Integer, expMeta :: (c, d) }
  | Double { doubleValue :: Double, expMeta :: (c, d) }
  | UnaryOperation { unOp :: UnaryOperator, inner :: Expression a b c d, expMeta :: (c, d) }
  | BinaryOperation { binOp :: BinaryOperator, left :: Expression a b c d, right :: Expression a b c d, expMeta :: (c, d) }
  | Variable { expName :: Name, expVarMeta :: b, expMeta :: (c, d) }
  | Call { expName :: Name, expArgs :: [Expression a b c d], expCallMeta :: a, expMeta :: (c, d) }
  | Conditional { cond :: Expression a b c d, thenExp :: Expression a b c d, elseExp :: Expression a b c d, expMeta :: (c, d) }
  | Block { stmts :: [Expression a b c d], inner :: Expression a b c d, expMeta :: (c, d) }
  | While { invariants :: [Expression a b c d], cond :: Expression a b c d, inner :: Expression a b c d, expMeta :: (c, d) }
  deriving (Eq, Ord, Show)

instance HasExpressionMeta (Expression a b c d) c where
  expressionMeta = fst . expMeta

instance HasNodeMeta (Expression a b c d) d where
  nodeMeta = snd . expMeta

-- | Program with 4 types of metadata:
-- [a:] The metadata used for signatures and calls.
-- [b:] The metadata used for variables and variable declarations.
-- [c:] The metadata used for all expressions.
-- [d:] The metadata used for all nodes.
data Program a b c d
  = Program { program :: [Declaration a b c d], progMeta :: d }
  deriving (Eq, Ord, Show)

instance HasNodeMeta (Program a b c d) d where
  nodeMeta = progMeta
