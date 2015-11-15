-- | Special variables and functions that are built in.

module Sara.Builtins ( BuiltinVar(..)
                     , stringToBuiltinVar
                     , name ) where

import Data.Char

data BuiltinVar
  = Result
  deriving (Eq, Ord, Show, Enum, Bounded)

stringToBuiltinVar :: String -> Maybe BuiltinVar
stringToBuiltinVar "result" = Just Result
stringToBuiltinVar _        = Nothing

name :: BuiltinVar -> String
name b = case show b of
  []   -> error "Empty builtin vars are not possible."
  x:xs -> toLower x : xs
