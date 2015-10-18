module Errors where

import Text.Parsec.Pos
import Control.Monad.Except
import qualified Text.Parsec.Error as E

type ErrorOr a = Except Error a

instance Eq E.ParseError where
  l == r = show l == show r

data Error
  = ParseError E.ParseError
  | TypeError SourcePos String
  deriving (Eq)

instance Show Error where
  show (ParseError err)    = show err
  show (TypeError pos msg) = show pos ++ ":\n" ++ msg

typeError :: String -> SourcePos -> ErrorOr a
typeError msg pos = throwError $ TypeError pos msg
