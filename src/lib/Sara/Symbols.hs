module Sara.Symbols where

import Sara.Syntax
import Sara.Types
import Sara.Meta
import qualified Data.Map.Strict as M

data FunctionKey =
  FunctionKey Name [Type]
  deriving (Eq, Ord, Show)

type FunctionMap = M.Map FunctionKey ParserSignature
type VariableMap = M.Map Name ParserTypedVariable
