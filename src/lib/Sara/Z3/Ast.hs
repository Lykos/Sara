{-# LANGUAGE PatternSynonyms #-}

module Sara.Z3.Ast ( Pattern(..)
                   , AppKind(..)
                   , AppMeta(..)
                   , Ast(..)
                   , UnaryOperator (..)
                   , BinaryOperator (..)
                   , NaryOperator (..)
                   , neutral
                   , children
                   , simplify
                   , substituteFuncs ) where

import Data.List
import qualified Data.Map.Strict as M
import Sara.Ast.Meta ( VariableMeta, FunctionMeta )

data AppKind
  = PreApp
  | PostApp
  | FuncApp
  | FakeApp
  deriving (Eq, Ord, Show, Enum, Bounded)

data AppMeta = AppMeta AppKind FunctionMeta
             deriving (Eq, Ord, Show)

newtype Pattern = Pattern [Ast]
                deriving (Eq, Ord, Show)

data Ast
  = BoolConst Bool
  | IntConst Integer
  | Var VariableMeta
  | App AppMeta [Ast]
  | UnOp UnaryOperator Ast
  | BinOp BinaryOperator Ast Ast
  | NaryOp NaryOperator [Ast]
  | Ite Ast Ast Ast
  | Forall [VariableMeta] Ast [Pattern]
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
  | Xor
  | Implies
  deriving (Eq, Ord, Show, Enum, Bounded)

data NaryOperator
  = Times
  | Plus
  | And
  | Or
  deriving (Eq, Ord, Show, Enum, Bounded)

neutral :: NaryOperator -> Ast
neutral Times = IntConst 1
neutral Plus  = IntConst 0
neutral And   = BoolConst True
neutral Or    = BoolConst False

children :: Ast -> [Ast]
children BoolConst{}      = []
children IntConst{}       = []
children Var{}            = []
children (App _ bs)       = bs
children (UnOp _ a)       = [a]
children (BinOp _ a b)    = [a, b]
children (NaryOp _ as)    = as
children (Ite a b c)      = [a, b, c]
children (Forall _ b _)   = [b]

simplify :: Ast -> Ast
simplify b@BoolConst{}    = b
simplify n@IntConst{}     = n
simplify v@Var{}          = v
simplify (App a bs)       = App a $ map simplify bs
simplify (UnOp op a)      = case UnOp op (simplify a) of
  UnOp UnMinus (IntConst n)             -> IntConst $ -n
  UnOp Not (BoolConst b)                -> BoolConst $ not b
  UnOp UnMinus (UnOp UnMinus a)         -> a
  UnOp Not (UnOp Not a)                 -> a
  u                                     -> u
simplify (BinOp op a b)   = case BinOp op (simplify a) (simplify b) of
  BinOp DividedBy (IntConst a) (IntConst b)    -> IntConst $ a `div` b
  BinOp Modulo (IntConst a) (IntConst b)       -> IntConst $ a `mod` b
  BinOp Minus (IntConst a) (IntConst b)        -> IntConst $ a - b
  BinOp LessThan (IntConst a) (IntConst b)     -> BoolConst $ a < b
  BinOp AtMost (IntConst a) (IntConst b)       -> BoolConst $ a <= b
  BinOp GreaterThan (IntConst a) (IntConst b)  -> BoolConst $ a > b
  BinOp AtLeast (IntConst a) (IntConst b)      -> BoolConst $ a >= b
  BinOp EqualTo (IntConst a) (IntConst b)      -> BoolConst $ a == b
  BinOp EqualTo (BoolConst a) (BoolConst b)    -> BoolConst $ a == b
  BinOp Xor (BoolConst a) (BoolConst b)        -> BoolConst $ a && not b || not a && b
  BinOp Implies (BoolConst a) (BoolConst b)    -> BoolConst $ not a || b
  b                                            -> b
simplify (NaryOp op xs)   = case NaryOp op (concatMap (extractOperator op) $ filter (/= neutral op) $ map simplify xs) of
  NaryOp op [] -> neutral op
  NaryOp _ [a] -> a
  a            -> a
  where extractOperator op (NaryOp op' as) | op == op' = as
        extractOperator _  a                           = [a]
simplify (Ite a b c)      = case Ite (simplify a) (simplify b) (simplify c) of
  Ite (BoolConst True) a _  -> a
  Ite (BoolConst False) _ b -> b
  a                         -> a
simplify (Forall as b cs) = case Forall as (simplify b) cs of
  Forall as (Forall as' b cs') cs -> Forall (as ++ as') b [Pattern (c ++ c') | Pattern c <- cs, Pattern c' <- cs']
  a                               -> a

-- | Substitutes vars with a given ast in a map from vars to asts.
substituteVars :: M.Map VariableMeta Ast -> Ast -> Ast
substituteVars _ (BoolConst b)  = BoolConst b
substituteVars _ (IntConst n)   = IntConst n
substituteVars m (Var a)        = case M.lookup a m of
  Just v  -> v
  Nothing -> Var a
substituteVars m (App a bs)       = App a $ map (substituteVars m) bs
substituteVars m (UnOp op a)      = UnOp op $ substituteVars m a
substituteVars m (BinOp op a b)   = BinOp op (substituteVars m a) (substituteVars m b)
substituteVars m (NaryOp op bs)   = NaryOp op $ map (substituteVars m) bs
substituteVars m (Ite a b c)      = Ite (substituteVars m a) (substituteVars m b) (substituteVars m c)
substituteVars m (Forall as b cs) = Forall as (substituteVars m' b) cs
  where m' = foldl (flip M.delete) m as

freeVars :: Ast -> [VariableMeta]
freeVars (Var a)         = [a]
freeVars (Forall as b _) = freeVars b \\ as
freeVars a               = concatMap freeVars $ children a

substituteArgs :: [VariableMeta] -> [Ast] -> Ast -> Ast
substituteArgs formalArgs args body | length formalArgs == length args = let argSubstitutions = M.fromList (zip formalArgs args)
                                                                             unsubstitutedArgs = freeVars body \\ formalArgs
                                                                             substituted = substituteVars argSubstitutions body
                                                                         in if null unsubstitutedArgs then
                                                                              substituted
                                                                            else
                                                                              error $ "Couldn't substitute the following variables:\n"
                                                                              ++ show unsubstitutedArgs
                                                                              ++ "\nwith the arguments:\n"
                                                                              ++ show formalArgs
                                                                              ++ "\nin the following expression:\n"
                                                                              ++ show body
                                    | otherwise                        =
 error $ "Function definition has formal arguments " ++ show formalArgs ++ " and actual arguments " ++ show args ++ " with different lengths."

-- | Substituted function by their body.
substituteFuncs :: M.Map AppMeta ([VariableMeta], Ast) -> Ast -> Ast
substituteFuncs _ b@BoolConst{}    = b
substituteFuncs _ n@IntConst{}     = n
substituteFuncs _ v@Var{}          = v
substituteFuncs m (App a bs)       = let args = map (substituteFuncs m) bs in case M.lookup a m of
  Just (formalArgs, body) -> substituteArgs formalArgs args body
  Nothing                 -> App a args
substituteFuncs m (UnOp op a)      = UnOp op $ substituteFuncs m a
substituteFuncs m (BinOp op a b)   = BinOp op (substituteFuncs m a) (substituteFuncs m b)
substituteFuncs m (NaryOp op bs)   = NaryOp op $ map (substituteFuncs m) bs
substituteFuncs m (Ite a b c)      = Ite (substituteFuncs m a) (substituteFuncs m b) (substituteFuncs m c)
substituteFuncs m (Forall as b cs) = Forall as (substituteFuncs m b) cs
