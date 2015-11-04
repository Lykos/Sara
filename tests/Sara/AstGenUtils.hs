{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sara.AstGenUtils ( identifier
                        , pos
                        , clearPositions
                        , clearSymbols
                        , testfile
                        , inferSignature
                        , completeProgram
                        , mkNodeMeta
                        , PureExpression(..) ) where

import Sara.ArbitraryUtils
import Sara.Syntax
import Sara.Symbolizer
import Sara.Types
import Sara.Lexer
import Sara.Meta
import Sara.Operators
import Sara.AstUtils
import Sara.GenT
import Sara.Utils
import qualified Sara.Syntax as S
import qualified Sara.Types as T

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.QuickCheck as Q
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Text.Parsec.Pos
import Data.Maybe
import qualified Data.Map.Strict as Map

instance (MonadState s m) => MonadState s (GenT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (Monoid w, MonadWriter w m) => MonadWriter w (GenT m) where
  writer = lift . writer
  tell   = lift . tell
  listen = mapGenT listen
  pass   = mapGenT pass

instance (MonadReader e m) => MonadReader e (GenT m) where
  ask = lift ask
  local = mapGenT . local
  reader = lift . reader

instance (MonadGen m) => MonadGen (StateT s m) where
  liftGen = lift . liftGen
  variant = mapStateT . variant
  sized f = StateT $ \state -> sized $ \size -> runStateT (f size) state
  resize = mapStateT . resize
  choose = lift . choose

instance (Monoid w, MonadGen m) => MonadGen (WriterT w m) where
  liftGen = lift . liftGen
  variant = mapWriterT . variant
  sized f = WriterT $ sized $ runWriterT . f
  resize = mapWriterT . resize
  choose = lift . choose

instance (MonadGen m) => MonadGen (ReaderT e m) where
  liftGen = lift . liftGen
  variant = mapReaderT . variant
  sized f = ReaderT $ \env -> sized $ \size -> runReaderT (f size) env
  resize = mapReaderT . resize
  choose = lift . choose

-- | Valid initial letters for an identifier.
identifierStarts :: String
identifierStarts = ['a'..'z'] ++ ['A'..'Z'] ++ "_"

-- | Valid intermediate letters for an identifier.
identifierLetters :: String
identifierLetters = identifierStarts ++ ['0'..'9']

arbitraryIdentifierCandidate :: MonadGen g => g Name
arbitraryIdentifierCandidate = do
  i0 <- elements identifierStarts
  iN <- listOf $ elements identifierLetters
  return (i0:iN)

arbitraryIdentifier :: (MonadGen g, MonadState GeneratorEnv g) => g Name
arbitraryIdentifier = do
  id <- arbitraryIdentifierCandidate
  valid <- isNotReserved id
  if valid then return id else arbitraryIdentifier

-- | Tests whether the identifier is not a reserved name.
isNotReserved :: MonadState GeneratorEnv m => Name -> m Bool
isNotReserved a = do
  names <- gets variables
  return $ a /= "main" && a `notElem` reservedNames && a `S.notMember` names

testfile :: String
testfile = "<testfile>"

position :: SourcePos
position = newPos testfile 0 0

mkNodeMeta :: NodeMeta
mkNodeMeta = NodeMeta position

mkExpMeta :: Type -> ExpMeta
mkExpMeta t = (ExpressionMeta t, NodeMeta position)

-- | Creates metadata for nodes that need two types of metadata. One unit metadata and one NodeMetadata.
mkNodePlusMeta :: ((), NodeMeta)
mkNodePlusMeta = ((), mkNodeMeta)

addExpMeta :: Monad g => Type -> g UntypedExpression -> g TypeCheckerExpression
addExpMeta t gen = gen <*> pure (mkExpMeta t)

-- | A signature where pre- and postconditions are missing.
data PartialSignature
  = PartialSignature { isPure :: Bool
                     , sigName :: Name
                     , args :: [TypeCheckerTypedVariable]
                     , retType :: Type
                     , sigMeta :: ((), NodeMeta) }

toSignature :: PartialSignature -> [TypeCheckerExpression] -> [TypeCheckerExpression] -> TypeCheckerSignature
toSignature PartialSignature{..} pres posts = Signature isPure sigName args retType pres posts sigMeta

-- | Environment for the generation of expressions. The methods and functions are keyed by return type.
data GeneratorEnv
  = GeneratorEnv { callables :: M.Map T.Type [PartialSignature] -- ^ In a pure context, these are the functions, otherwise, functions and methods.
                 , functions :: M.Map T.Type [PartialSignature] -- ^ Pure functions.
                 , variables :: M.Map T.Type [Name]
                 , isPureEnv :: Bool }

initialEnv :: [PartialSignature] -> GeneratorEnv
initialEnv sigs = GeneratorEnv { callables = callables'
                               , functions = functions'
                               , variables = M.empty
                               , isPureEnv = undefined }
  where callables' = keyBy Sara.AstGenUtils.retType sigs
        functions' = M.map (filter Sara.AstGenUtils.isPure) callables'

arbitraryProgram :: MonadGen g => g TypeCheckerProgram
arbitraryProgram = scale intRoot $ do
  sigs <- evalStateT (listOf arbitraryPartialSignature) S.empty
  decls <- runReaderT (mapM arbitraryDeclForSignature sigs) (initialEnv sigs)
  return $ Program decls mkNodeMeta

partialFunctionKey :: PartialSignature -> FunctionKey
partialFunctionKey PartialSignature{..} = FunctionKey sigName (map S.varType args)

arbitraryPartialSignature :: (MonadGen g, MonadState (S.Set FunctionKey) g) => g PartialSignature
arbitraryPartialSignature = do
  sigs <- get
  sig <- arbitrarySignature' `suchThat` (\sig -> partialFunctionKey sig `S.notMember` sigs)
  modify $ S.insert $ partialFunctionKey sig
  return sig
  where arbitrarySignature' = do
          pur <- arbitraryBool
          nam <- arbitraryIdentifier
          args <- evalStateT (scale intRoot $ listOf $ arbitraryTypedVariable) S.empty
          retType <- arbitraryType
          return $ PartialSignature pur nam args retType mkNodePlusMeta

arbitraryTypedVariable :: (MonadGen g, MonadState GeneratorEnv g) => g TypeCheckerTypedVariable
arbitraryTypedVariable = do
  names <- gets variables
  name <- arbitraryIdentifier `suchThat` flip S.notMember names
  modify $ S.insert name
  typ <- arbitraryType
  return $ TypedVariable name typ mkNodePlusMeta

arbitraryType :: MonadGen g => g Type
arbitraryType = elements [T.Unit, T.Boolean, T.Integer, T.Double]

-- | The transformation of the environment that a partial signature causes, i.e. the arguments are added to the local variables.
partialSigEnvTransform :: PartialSignature -> GeneratorEnv -> GeneratorEnv
partialSigEnvTransform sig env = env{ variables = variables', callables = callables', isPureEnv = Sara.AstGenUtils.isPure sig }
  where as :: M.Map Type [Name]
        as = M.map (map S.varName) $ keyBy S.varType $ Sara.AstGenUtils.args sig
        variables' = M.unionWith (++) (variables env) as
        callables' = if Sara.AstGenUtils.isPure sig then callables env else functions env

arbitrarySignatureForPartialSig :: (MonadGen g, MonadReader GeneratorEnv g) => PartialSignature -> g TypeCheckerSignature
arbitrarySignatureForPartialSig sig = toSignature sig <$> conditions <*> conditions
  where conditions = local (partialSigEnvTransform sig) $ scale intRoot $ listOf $ arbitraryExpression T.Boolean

arbitrarySignature :: MonadGen g => g TypeCheckerSignature
arbitrarySignature = do
  sig <- evalStateT arbitraryPartialSignature S.empty
  runReaderT (arbitrarySignatureForPartialSig sig) (initialEnv [sig])

arbitraryDeclForSignature :: (MonadGen g, MonadReader GeneratorEnv g) => PartialSignature -> g TypeCheckerDeclaration
arbitraryDeclForSignature partialSig = do
  sig <- arbitrarySignatureForPartialSig partialSig
  body <- local (partialSigEnvTransform partialSig) $ arbitraryExpression $ S.retType sig
  return $ Function sig body mkNodeMeta

arbitraryDeclaration :: MonadGen g => g TypeCheckerDeclaration
arbitraryDeclaration = do
  sig <- evalStateT arbitraryPartialSignature S.empty
  runReaderT (arbitraryDeclForSignature sig) (initialEnv [sig])

type ExpMeta = (ExpressionMeta, NodeMeta)
type UntypedExpression = ExpMeta -> TypeCheckerExpression

arbitraryBoolean :: MonadGen g => g UntypedExpression
arbitraryBoolean = S.Boolean <$> arbitraryBool

-- | Generates a positive integer expression.
-- We don't like negative numbers here because -4 gets parsed as 4 with the unary minus applied to it.
arbitraryInteger :: MonadGen g => g UntypedExpression
arbitraryInteger = S.Integer <$> arbitrarySizedNatural

arbitraryDouble :: MonadGen g => g UntypedExpression
arbitraryDouble = liftM S.Double $ elements niceDoubles
  where niceDoubles = [0.0, 0.1, 1.0, 1.1, 1e10, 1.1e10]

arbitraryVariable :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe UntypedExpression)
arbitraryVariable t = do
  vars <- asks $ M.lookup t . variables
  case vars of
    Nothing   -> return Nothing
    Just []   -> return Nothing
    Just vs -> do
      v <- elements vs
      return $ Just $ Variable v ()

arbitraryCall :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe UntypedExpression)
arbitraryCall t = do
  calls <- asks $ M.lookup t . callables
  case calls of
    Nothing    -> return Nothing
    Just []    -> return Nothing
    Just calls -> do
      PartialSignature{..} <- elements calls
      as <- arbitraryArgs args
      return $ Just $ Call sigName as ()

-- | Generates expressions as arguments for a function or method call given a list of desired typed variables
-- representing the formal arguments of the function or method to call.
arbitraryArgs :: (MonadGen g, MonadReader GeneratorEnv g) => [TypeCheckerTypedVariable] -> g [TypeCheckerExpression]
arbitraryArgs args = scale (`div` numArgs) $ mapM (arbitraryExpression . varType) args
  where numArgs = length args

intRoot :: Int -> Int
intRoot = round . sqrt . fromIntegral

invert :: (Ord k, Ord v) => M.Map k v -> M.Map v [k]
invert m = M.fromListWith (++) pairs
  where pairs = [(v, [k]) | (k, v) <- M.toList m]

invertedUnOps :: M.Map Type [TypedUnOp]
invertedUnOps = invert typedUnOps

invertedBinOps :: M.Map Type [TypedBinOp]
invertedBinOps = invert typedBinOps

findListWithDefault :: Ord k => M.Map k [v] -> k -> [v]
findListWithDefault map key = M.findWithDefault [] key map

typUnOps :: Type -> [TypedUnOp]
typUnOps = findListWithDefault invertedUnOps
          
typBinOps :: Type -> [TypedBinOp]
typBinOps = findListWithDefault invertedBinOps

arbitraryAssignable :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe TypeCheckerExpression)
arbitraryAssignable t = do
  var <- arbitraryVariable t
  case var of
    Just v  -> return $ Just $ v (mkExpMeta t)
    Nothing -> return Nothing

-- | Creates one generator for each possible binary operator for that type.
arbitraryBinaryOperations :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g [UntypedExpression]
arbitraryBinaryOperations t = do
  ops <- mapM binOp $ typBinOps t
  return $ catMaybes ops
  where binOp :: (MonadGen g, MonadReader GeneratorEnv g) => TypedBinOp -> g (Maybe UntypedExpression)
        binOp (TypedBinOp Assign r s) = do
          pure <- asks isPureEnv
          var <- arbitraryAssignable r
          exp <- arbitraryExpression s
          case var of
            Just v  | pure -> return $ Just $ BinaryOperation Assign v exp
            _              -> return Nothing
        binOp (TypedBinOp op r s)     = Just <$> (BinaryOperation op <$> subtree r <*> subtree s)
        subtree r = scale (`div` 2) $ arbitraryExpression r
                                 
-- | Creates one generator for each possible unary operator for that type.
arbitraryUnaryOperations :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g [UntypedExpression]
arbitraryUnaryOperations t = mapM unOp $ typUnOps t
  where unOp (TypedUnOp op s) = UnaryOperation op <$> subtree s
        subtree s = scale pred $ arbitraryExpression s

arbitraryConditional :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g UntypedExpression
arbitraryConditional t = Conditional <$> subtree T.Boolean <*> subtree t <*> subtree t
  where subtree t = scale (`div` 3) $ arbitraryExpression t

arbitraryBlock :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g UntypedExpression
arbitraryBlock t = Block <$> stmts <*> exp
  where stmts = scale intRoot $ listOf $ arbitraryType >>= arbitraryExpression
        exp = scale intRoot $ arbitraryExpression t

arbitraryWhile :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe UntypedExpression)
arbitraryWhile t = do
  isPure <- asks isPureEnv
  case t of
    T.Unit | not isPure -> Just <$> (While <$> subtree T.Boolean <*> (arbitraryType >>= subtree))
    _                   -> return Nothing
  where subtree t = scale (`div` 2) $ arbitraryExpression t

arbitraryLeafExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g UntypedExpression
arbitraryLeafExpression t = do
  var <- arbitraryVariable t
  c <- arbitraryConstant t
  case var of
    Nothing -> return c
    Just v  -> elements [v, c]

arbitraryConstant :: MonadGen g => Type -> g UntypedExpression
arbitraryConstant T.Boolean = arbitraryBoolean
arbitraryConstant T.Integer = arbitraryInteger
arbitraryConstant T.Double  = arbitraryDouble
arbitraryConstant T.Unit    = return S.Unit

arbitraryInnerExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g UntypedExpression
arbitraryInnerExpression t = do
  binOps <- arbitraryBinaryOperations t
  unOps <- arbitraryUnaryOperations t
  let numUnOps = length unOps
  let numBinOps = length binOps
  let weightUnOps = if numBinOps == 0 then 1 else numBinOps
  let weightBinOps = if numUnOps == 0 then 1 else numUnOps
  let weightOthers = case (numUnOps, numBinOps) of
        (0, 0) -> 1
        (0, k) -> k
        (k, 0) -> k
        (k, l) -> k * l
  unreliableArbitraries <- catMaybes <$> sequence [arbitraryCall t, arbitraryWhile t]
  let anyTyped = map ($ t) [arbitraryLeafExpression, arbitraryLeafExpression, arbitraryConditional, arbitraryBlock]
                 ++ map return unreliableArbitraries
  -- We want the same probability to get a unary operation, a binary operation, a constant, a variable, a call or a conditional
  let weighted = map ((,) weightOthers) anyTyped
                 ++ map ((,) weightBinOps . return) binOps
                 ++ map ((,) weightUnOps . return) unOps
  frequency weighted

arbitraryExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g TypeCheckerExpression
arbitraryExpression t = addExpMeta t $ sized expression'
  where expression' :: (MonadGen g, MonadReader GeneratorEnv g) => Int -> g UntypedExpression
        expression' 0         = arbitraryLeafExpression t
        expression' n | n > 0 = arbitraryInnerExpression t
        expression' s         = error $ "expression' for negative size " ++ show s ++ " not supported."

-- | The most simple expression with a given type. Used for variable shrinking.
trivial :: ExpMeta -> TypeCheckerExpression
trivial m = trivial' (expTyp $ fst m) m
  where trivial' T.Boolean = S.Boolean False
        trivial' T.Integer = S.Integer 0
        trivial' T.Double  = S.Double 0.0
        trivial' T.Unit    = S.Unit

-- | Returns the free variables in an expression. Used to determine which declarations are shrinkable.
freeVariables :: TypeCheckerExpression -> S.Set TypeCheckerTypedVariable
freeVariables = foldMapExpression freeVariable
  where freeVariable v@(Variable a _ _) = S.singleton $ TypedVariable a (expressionTyp v) ((), mkNodeMeta)
        freeVariable _                  = S.empty

-- | Returns the called functions in a program. Used to determine which signatures are shrinkable.
calledFunctions :: TypeCheckerProgram -> S.Set FunctionKey
calledFunctions = foldMapExpressions calledFunctionsExpression
  where calledFunctionsExpression c@Call{} = S.singleton $ callFunctionKey c
        calledFunctionsExpression _        = S.empty

shrinkExpression :: TypeCheckerExpression -> [TypeCheckerExpression]
shrinkExpression b@S.Boolean{ boolValue = val }     = [b{ boolValue = v } | v <- Q.shrink val]
shrinkExpression n@S.Integer{ intValue = val }      = [n{ intValue = v } | v <- Q.shrink val]
shrinkExpression d@S.Double{ doubleValue = val }    = [d{ doubleValue = v } | v <- Q.shrink val]
shrinkExpression S.Unit{}                           = []
shrinkExpression (Variable _ _ m)                   = [trivial m]
shrinkExpression (BinaryOperation op left right m)  = childrenWithType m [left, right]
                                                      ++ [BinaryOperation op l r m | (l, r) <- Q.shrink (left, right)]
shrinkExpression (UnaryOperation op exp m)          = childrenWithType m [exp]
                                                      ++ [UnaryOperation op e m | e <- Q.shrink exp]
shrinkExpression (Call name args cm m)              = childrenWithType m args
                                                      ++ [Call name a cm m | a <- shrinkArgs args]
  where shrinkArgs :: [TypeCheckerExpression] -> [[TypeCheckerExpression]]
        shrinkArgs []     = []
        shrinkArgs (x:xs) = [y : xs | y <- Q.shrink x] ++ [x : ys | ys <- shrinkArgs xs]
shrinkExpression (Conditional cond ifExp elseExp m) = childrenWithType m [cond, ifExp, elseExp]
                                                      ++ [Conditional c i e m | (c, i, e) <- Q.shrink (cond, ifExp, elseExp)]
shrinkExpression (Block stmts exp m)                = [exp]
                                                      ++ childrenWithType m stmts
                                                      ++ [Block (init stmts) (last stmts) m | not (null stmts), expressionTyp (last stmts) == expTyp (fst m)]
                                                      ++ [Block s e m | (s, e) <- Q.shrink (stmts, exp)]
shrinkExpression (While cond body m)                = While cond (S.Unit (ExpressionMeta T.Unit, mkNodeMeta)) m
                                                      : [While c b m | (c, b) <- Q.shrink (cond, body)]

childrenWithType :: ExpMeta -> [TypeCheckerExpression] -> [TypeCheckerExpression]
childrenWithType m = filter (\c -> expressionTyp c == (expTyp $ fst m))

shrinkSignature :: Bool -> S.Set ParserTypedVariable -> TypeCheckerSignature -> [TypeCheckerSignature]
shrinkSignature isRemovable free (Signature pure name args typ precs posts p) =
  [Signature pure name args typ precs' posts' p | (precs', posts') <- Q.shrink (precs, posts)]
  ++ [Signature pure name a typ precs posts p | a <- shrinkArgTypes args]
  where shrinkArgTypes :: [ParserTypedVariable] -> [[ParserTypedVariable]]
        shrinkArgTypes []                         = []
        shrinkArgTypes (x:xs) | x `S.member` free = [x : ys | ys <- shrinkArgTypes xs]
                              | otherwise         = [xs | isRemovable]  -- We can only remove arguments if the function is never called.
                                                    ++ [x : ys | ys <- shrinkArgTypes xs]

isRemovableSignature :: S.Set FunctionKey -> TypeCheckerSignature -> Bool
isRemovableSignature funcs sig = functionKey sig `S.notMember` funcs

condFreeVariables :: TypeCheckerSignature -> S.Set TypeCheckerTypedVariable
condFreeVariables Signature{..} = freeVars preconditions `S.union` freeVars postconditions
  where freeVars = foldr S.union S.empty . map freeVariables

shrinkDeclaration :: S.Set FunctionKey -> TypeCheckerDeclaration -> [TypeCheckerDeclaration]
shrinkDeclaration funcs (Function sig body meta) = let
  isRemovable = isRemovableSignature funcs sig
  free = condFreeVariables sig `S.union` freeVariables body
  in [Function s body meta | s <- shrinkSignature isRemovable free sig]
     ++ [Function sig b meta | b <- shrinkExpression body]
shrinkDeclaration funcs (Extern sig meta) = let
  isRemovable = isRemovableSignature funcs sig
  free = condFreeVariables sig
  in [Extern s meta | s <- shrinkSignature isRemovable free sig]

shrinkProgram :: TypeCheckerProgram -> [TypeCheckerProgram]
shrinkProgram p = shrinkProgram' (calledFunctions p) p
  where meta = S.progMeta p
        shrinkProgram' _ (Program [] _)         = []
        shrinkProgram' funcs (Program (x:xs) _) = headRemovals ++ map appendTail headShrinks ++ map appendHead tailShrinks
          where headShrinks = shrinkDeclaration funcs x
                tailShrinks = shrinkProgram' funcs $ Program xs meta
                headRemovals = [Program xs meta | isRemovableDeclaration funcs x]
                isRemovableDeclaration funcs = isRemovableSignature funcs . signature
                appendTail y = Program (y:xs) meta
                appendHead (Program ys p) = Program (x : ys) p

emptyEnvWithPureness :: Bool -> GeneratorEnv
emptyEnvWithPureness isPure = GeneratorEnv{ callables = M.empty, functions = M.empty, variables = M.empty, isPureEnv = isPure }

newtype PureExpression
  = PureExpression { runPureExpression :: TypeCheckerExpression }
  deriving (Eq, Ord, Show)

instance Q.Arbitrary TypeCheckerExpression where
  arbitrary = do
    typ <- arbitraryType
    pure <- choose (False, True)
    evalStateT (arbitraryExpression typ) (emptyEnvWithPureness pure)
  shrink = shrinkExpression

instance Q.Arbitrary PureExpression where
  arbitrary = do
    typ <- arbitraryType
    PureExpression <$> runReaderT (arbitraryExpression typ) (emptyEnvWithPureness True)
  shrink = map PureExpression . shrinkExpression . runPureExpression

instance Q.Arbitrary TypeCheckerTypedVariable where
  arbitrary = evalStateT arbitraryTypedVariable S.empty
  shrink = Q.shrinkNothing

instance Q.Arbitrary TypeCheckerSignature where
  arbitrary = arbitrarySignature
  shrink = shrinkSignature True S.empty

instance Q.Arbitrary TypeCheckerDeclaration where
  arbitrary = arbitraryDeclaration
  shrink = shrinkDeclaration S.empty

instance Q.Arbitrary TypeCheckerProgram where
  arbitrary = arbitraryProgram
  shrink = shrinkProgram

instance Q.Arbitrary Type where
  arbitrary = arbitraryType

instance Q.Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Q.Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators
