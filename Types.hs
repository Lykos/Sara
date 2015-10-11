module Types where

data Type
  = Integer
  | Boolean
  | Double
  | Unknown
  deriving (Eq, Ord, Show)
