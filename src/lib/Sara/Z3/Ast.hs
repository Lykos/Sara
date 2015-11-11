{-# LANGUAGE PatternSynonyms #-}

module Sara.Z3.Ast ( Ast(..)
                   , UnaryOperator (..)
                   , BinaryOperator (..)
                   , NaryOperator (..)
                   , simplify
                   , substituteFuncs ) where

import qualified Data.Map.Strict as M

data Ast a b
  = BoolConst Bool
  | IntConst Integer
  | Var a
  | App b [Ast a b]
  | UnOp UnaryOperator (Ast a b)
  | BinOp BinaryOperator (Ast a b) (Ast a b)
  | NaryOp NaryOperator [Ast a b]
  | Ite (Ast a b) (Ast a b) (Ast a b)
  deriving (Eq, Ord, Show)

data UnaryOperator
  = UnMinus
  | Not
  deriving (Eq, Ord, Show, Enum, Bounded)

data BinaryOperator
  = DividedBy
  | Modulo
  | Minus
  | LessThan
  | AtMost
  | GreaterThan
  | AtLeast
  | EqualTo
  | NotEqualTo
  | Xor
  | Implies
  | ImpliedBy
  deriving (Eq, Ord, Show, Enum, Bounded)

data NaryOperator
  = Times
  | Plus
  | And
  | Or
  deriving (Eq, Ord, Show, Enum, Bounded)

neutral :: NaryOperator -> Ast a b
neutral Times = IntConst 1
neutral Plus  = IntConst 0
neutral And   = BoolConst True
neutral Or    = BoolConst False

simplify :: (Eq a, Eq b) => Ast a b -> Ast a b
simplify b@BoolConst{}  = b
simplify n@IntConst{}   = n
simplify v@Var{}        = v
simplify (App a b)      = App a $ map simplify b
simplify (UnOp op a)    = case UnOp op (simplify a) of
  UnOp UnMinus (IntConst n)             -> IntConst $ -n
  UnOp Not (BoolConst b)                -> BoolConst $ not b
  UnOp UnMinus (UnOp UnMinus a)         -> a
  UnOp Not (UnOp Not a)                 -> a
  u                                     -> u
simplify (BinOp op a b) = case BinOp op (simplify a) (simplify b) of
  BinOp DividedBy (IntConst a) (IntConst b)    -> IntConst $ a `div` b
  BinOp Modulo (IntConst a) (IntConst b)       -> IntConst $ a `mod` b
  BinOp Minus (IntConst a) (IntConst b)        -> IntConst $ a - b
  BinOp LessThan (IntConst a) (IntConst b)     -> BoolConst $ a < b
  BinOp AtMost (IntConst a) (IntConst b)       -> BoolConst $ a <= b
  BinOp GreaterThan (IntConst a) (IntConst b)  -> BoolConst $ a > b
  BinOp AtLeast (IntConst a) (IntConst b)      -> BoolConst $ a >= b
  BinOp EqualTo (IntConst a) (IntConst b)      -> BoolConst $ a == b
  BinOp EqualTo (BoolConst a) (BoolConst b)    -> BoolConst $ a == b
  BinOp NotEqualTo (IntConst a) (IntConst b)   -> BoolConst $ a /= b
  BinOp NotEqualTo (BoolConst a) (BoolConst b) -> BoolConst $ a /= b
  BinOp Xor (BoolConst a) (BoolConst b)        -> BoolConst $ a && not b || not a && b
  BinOp Implies (BoolConst a) (BoolConst b)    -> BoolConst $ not a || b
  BinOp ImpliedBy (BoolConst a) (BoolConst b)  -> BoolConst $ a || not b
  b                                            -> b
simplify (NaryOp op xs) = case NaryOp op (filter (/= neutral op) (map simplify xs)) of
  NaryOp op [] -> neutral op
  NaryOp _ [a] -> a
  a            -> a
simplify (Ite a b c)    = case Ite (simplify a) (simplify b) (simplify c) of
  Ite (BoolConst True) a _  -> a
  Ite (BoolConst False) _ b -> b
  a                         -> a

-- | Substitutes vars with a given ast in a map from vars to asts.
-- This expects that there is a substitution for all variables, otherwise it errors.
substituteVars :: (Ord a, Show a) => M.Map a (Ast b c) -> Ast a c -> Ast b c
substituteVars _ (BoolConst b)  = BoolConst b
substituteVars _ (IntConst n)   = IntConst n
substituteVars m (Var a)        = case M.lookup a m of
  Just v  -> v
  Nothing -> error $ "Unable to substitute variable '" ++ show a ++ "'."
substituteVars m (App a bs)     = App a $ map (substituteVars m) bs
substituteVars m (UnOp op a)    = UnOp op $ substituteVars m a
substituteVars m (BinOp op a b) = BinOp op (substituteVars m a) (substituteVars m b)
substituteVars m (NaryOp op bs) = NaryOp op $ map (substituteVars m) bs
substituteVars m (Ite a b c)    = Ite (substituteVars m a) (substituteVars m b) (substituteVars m c)

substituteFuncs :: (Ord a, Ord b, Show a, Show b, Show c) => M.Map a ([b], Ast b a) -> Ast c a -> Ast c a
substituteFuncs _ b@BoolConst{}  = b
substituteFuncs _ n@IntConst{}   = n
substituteFuncs _ v@Var{}        = v
substituteFuncs m (App a bs)     = let args = map (substituteFuncs m) bs in case M.lookup a m of
  Just (formalArgs, body) | length formalArgs == length args -> let argSubstitutions = M.fromList (zip formalArgs args) in substituteVars argSubstitutions body
                          | otherwise                        -> error $ "Function definition has formal arguments " ++ show formalArgs
                                                                ++ ", but it is called with the wrong number of arguments " ++ show args ++ "."
  Nothing                                                    -> App a args
substituteFuncs m (UnOp op a)    = UnOp op $ substituteFuncs m a
substituteFuncs m (BinOp op a b) = BinOp op (substituteFuncs m a) (substituteFuncs m b)
substituteFuncs m (NaryOp op bs) = NaryOp op $ map (substituteFuncs m) bs
substituteFuncs m (Ite a b c)    = Ite (substituteFuncs m a) (substituteFuncs m b) (substituteFuncs m c)

