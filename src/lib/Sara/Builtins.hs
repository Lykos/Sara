-- | Special variables and functions that are built in.

module Sara.Builtins where

data BuiltinVar
  = Result
  deriving (Eq, Ord, Show, Enum, Bounded)

builtinVar :: String -> Maybe BuiltinVar
builtinVar "result" = Just Result
builtinVar _        = Nothing
