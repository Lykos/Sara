module Sara.Parser.Keywords where

import Data.Char

data Keyword
  = Function
  | Extern
  | Method
  | If
  | Then
  | Else
  | While
  | True
  | False
  | Requires
  | Ensures
  | Assert
  | Assume
  | AssertAndCollapse
  | Invariant
  | Val
  | Var
  deriving (Eq, Ord, Show, Enum, Bounded)

keyword :: Keyword -> String
keyword word = case show word of
  []   -> error "Empty keywords are not possible."
  x:xs -> toLower x : xs

keywords :: [String]
keywords = map keyword $ enumFrom minBound
