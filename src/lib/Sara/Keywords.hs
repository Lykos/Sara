module Sara.Keywords where

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
  deriving (Eq, Ord, Show, Enum, Bounded)

keyword :: Keyword -> String
keyword = map toLower . show

keywords :: [String]
keywords = map keyword $ enumFrom minBound
