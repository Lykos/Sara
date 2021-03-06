{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sara.TestUtils.AstGenUtils () where

import Sara.TestUtils.ArbitraryUtils
import Sara.Ast.Syntax
import Sara.Semantic.Symbolizer
import Sara.Ast.Types
import Sara.Parser.Lexer
import Sara.Ast.Meta
import Sara.Ast.Operators
import Sara.Ast.AstUtils
import Sara.TestUtils.GenT
import Sara.Utils
import qualified Sara.Ast.Syntax as S
import qualified Sara.Ast.Types as T

import qualified Data.Set as S
import qualified Test.QuickCheck as Q
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M
import Sara.TestUtils.AstTestUtils

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

arbitraryIdentifier :: (MonadGen g, MonadState (S.Set Name) g) => g Name
arbitraryIdentifier = do
  id <- arbitraryIdentifierCandidate
  names <- get
  valid <- runReaderT (isNotReserved id) names
  when valid (modify $ S.insert id)
  if valid then return id else arbitraryIdentifier

-- | Shrinks an identifier given a set of reserved names.
shrinkIdentifier :: MonadReader (S.Set Name) m => Name -> m [Name]
shrinkIdentifier n = filterM isNotReserved $ filter validIdentifier (Q.shrink n)
  where validIdentifier []    = False
        validIdentifier (x:_) = x `elem` identifierStarts

-- | Tests whether the identifier is not a reserved name.
isNotReserved :: MonadReader (S.Set Name) m => Name -> m Bool
isNotReserved a = do
  names <- ask
  return $ a /= "main" && a `notElem` reservedNames && a `S.notMember` names

addExpMeta :: Monad g => Type -> g IncompleteExpression -> g TypeCheckerExpression
addExpMeta t gen = gen <*> pure (TypMeta t) <*> pure mkNodeMeta

-- | A signature where pre- and postconditions are missing.
data PartialSignature
  = PartialSignature { isPureSig :: Bool
                     , sigName :: Name
                     , args :: [TypeCheckerTypedVariable]
                     , retType :: Type }
  deriving (Eq, Ord, Show)

toSignature :: PartialSignature -> [TypeCheckerExpression] -> [TypeCheckerExpression] -> TypeCheckerSignature
toSignature PartialSignature{..} pres posts = Signature isPureSig sigName args retType pres posts () mkNodeMeta

-- | Environment for the generation of expressions. The methods and functions are keyed by return type.
data GeneratorEnv
  = GeneratorEnv { callables :: M.Map T.Type [PartialSignature] -- ^ In a pure context, these are the functions, otherwise, functions and methods.
                 , functions :: M.Map T.Type [PartialSignature] -- ^ Pure functions.
                 , variables :: M.Map T.Type [Name]
                 , isPureEnv :: Bool }
  deriving (Eq, Ord, Show)

initialEnv :: [PartialSignature] -> GeneratorEnv
initialEnv sigs = GeneratorEnv { callables = callables'
                               , functions = functions'
                               , variables = M.empty
                               , isPureEnv = undefined }
  where callables' = keyBy Sara.TestUtils.AstGenUtils.retType sigs
        functions' = M.map (filter isPureSig) callables'

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
          nam <- evalStateT arbitraryIdentifier S.empty
          args <- evalStateT (scale intRoot $ listOf $ arbitraryTypedVariable) S.empty
          retType <- arbitraryType
          return $ PartialSignature pur nam args retType

arbitraryTypedVariable :: (MonadGen g, MonadState (S.Set Name) g) => g TypeCheckerTypedVariable
arbitraryTypedVariable = do
  name <- arbitraryIdentifier
  typ <- arbitraryType
  return $ TypedVariable name typ () mkNodeMeta

shrinkTypedVariable :: MonadReader (S.Set Name) m => TypeCheckerTypedVariable -> m [TypeCheckerTypedVariable]
shrinkTypedVariable (TypedVariable v t m p) = do
  vs <- shrinkIdentifier v
  return [TypedVariable v' t m p | v' <- vs]

arbitraryType :: MonadGen g => g Type
arbitraryType = elements [T.Unit, T.Boolean, T.Integer, T.Double]

-- | The transformation of the environment that a partial signature causes, i.e. the arguments are added to the local variables.
-- The argument pure indicates whether it should be a pure context.
-- Note that this is not equivalent to the signature being pure since methods also have pure pre- and postconditions.
partialSigEnvTransform :: Bool -> PartialSignature -> GeneratorEnv -> GeneratorEnv
partialSigEnvTransform pure sig env = envWithPureness pure $ env{ variables = variables' }
  where as = M.map (map S.varName) $ keyBy S.varType $ Sara.TestUtils.AstGenUtils.args sig
        variables' = M.unionWith (++) (variables env) as

arbitrarySignatureForPartialSig :: (MonadGen g, MonadReader GeneratorEnv g) => PartialSignature -> g TypeCheckerSignature
arbitrarySignatureForPartialSig sig = toSignature sig <$> conditions <*> conditions
  where conditions = local envTransform $ scale intRoot $ listOf $ arbitraryExpression T.Boolean
        envTransform env = partialSigEnvTransform True sig env

arbitrarySignature :: MonadGen g => g TypeCheckerSignature
arbitrarySignature = do
  sig <- evalStateT arbitraryPartialSignature S.empty
  runReaderT (arbitrarySignatureForPartialSig sig) (initialEnv [sig])

arbitraryDeclForSignature :: (MonadGen g, MonadReader GeneratorEnv g) => PartialSignature -> g TypeCheckerDeclaration
arbitraryDeclForSignature partialSig = do
  sig <- arbitrarySignatureForPartialSig partialSig
  body <- local (partialSigEnvTransform (isPureSig partialSig) partialSig) $ arbitraryExpression $ S.retType sig
  return $ Function sig body mkNodeMeta

arbitraryDeclaration :: MonadGen g => g TypeCheckerDeclaration
arbitraryDeclaration = do
  sig <- evalStateT arbitraryPartialSignature S.empty
  runReaderT (arbitraryDeclForSignature sig) (initialEnv [sig])

type IncompleteExpression = TypMeta -> NodeMeta -> TypeCheckerExpression

arbitraryBoolean :: MonadGen g => g IncompleteExpression
arbitraryBoolean = S.Boolean <$> arbitraryBool

-- | Generates a positive integer expression.
-- We don't like negative numbers here because -4 gets parsed as 4 with the unary minus applied to it.
arbitraryInteger :: MonadGen g => g IncompleteExpression
arbitraryInteger = S.Integer <$> arbitrarySizedNatural

arbitraryDouble :: MonadGen g => g IncompleteExpression
arbitraryDouble = liftM S.Double $ elements niceDoubles
  where niceDoubles = [0.0, 0.1, 1.0, 1.1, 1e10, 1.1e10]

arbitraryVariable :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe IncompleteExpression)
arbitraryVariable t = do
  vars <- asks $ M.lookup t . variables
  case vars of
    Nothing   -> return Nothing
    Just []   -> return Nothing
    Just vs -> do
      v <- elements vs
      return $ Just $ Variable v ()

arbitraryAssertionKind :: MonadGen g => g AssertionKind
arbitraryAssertionKind = elements assertionKinds

arbitraryAssertion :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe IncompleteExpression)
arbitraryAssertion t = do
  pure <- asks isPureEnv
  case t of
    T.Unit | not pure -> liftM Just $ Assertion <$> arbitraryAssertionKind <*> local (envWithPureness True) (arbitraryExpression T.Boolean)
    _                 -> return Nothing

arbitraryCall :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe IncompleteExpression)
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
    Just v  -> Just <$> (addExpMeta t $ return v)
    Nothing -> return Nothing

-- | Creates one generator for each possible binary operator for that type.
arbitraryBinaryOperations :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g [IncompleteExpression]
arbitraryBinaryOperations t = do
  ops <- mapM binOp $ typBinOps t
  return $ catMaybes ops
  where binOp :: (MonadGen g, MonadReader GeneratorEnv g) => TypedBinOp -> g (Maybe IncompleteExpression)
        binOp (TypedBinOp Assign r s) = do
          pure <- asks isPureEnv
          var <- arbitraryAssignable r
          exp <- arbitraryExpression s
          case var of
            Just v  | not pure -> return $ Just $ BinaryOperation Assign v exp
            _                  -> return Nothing
        binOp (TypedBinOp op r s)     = Just <$> (BinaryOperation op <$> subtree r <*> subtree s)
        subtree r = scale (`div` 2) $ arbitraryExpression r
                                 
-- | Creates one generator for each possible unary operator for that type.
arbitraryUnaryOperations :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g [IncompleteExpression]
arbitraryUnaryOperations t = mapM unOp $ typUnOps t
  where unOp (TypedUnOp op s) = UnaryOperation op <$> subtree s
        subtree s = scale pred $ arbitraryExpression s

arbitraryConditional :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g IncompleteExpression
arbitraryConditional t = Conditional <$> subtree T.Boolean <*> subtree t <*> subtree t
  where subtree t = scale (`div` 3) $ arbitraryExpression t

arbitraryBlock :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g IncompleteExpression
arbitraryBlock t = do
  newVars <- evalStateT (scale ((`div` 2) . intRoot) $ listOf $ arbitraryTypedVariable) S.empty
  arbitraryBlockWithNewVars [] newVars
  where arbitraryBlockWithNewVars defs (var:vars) = do
          exp' <- local (withoutVarEnvTransform var) $ arbitraryExpression (varType var)
          pure <- asks isPureEnv
          isVal <- if pure then return True else arbitraryBool
          def <- addExpMeta T.Unit $ return $ VarDef var isVal exp'
          local (varEnvTransform var) $ arbitraryBlockWithNewVars (def : defs) vars
        arbitraryBlockWithNewVars defs []         = do
          stmts' <- stmts
          exp' <- exp
          return $ Block (defs ++ stmts') exp'
        withoutVarEnvTransform :: TypeCheckerTypedVariable -> GeneratorEnv -> GeneratorEnv
        withoutVarEnvTransform var env = env{ variables = M.adjust (delete $ varName var) (varType var) (variables env) }
        varEnvTransform :: TypeCheckerTypedVariable -> GeneratorEnv -> GeneratorEnv
        varEnvTransform var env = env{ variables = M.insertWith (++) (varType var) [varName var] (variables env) }
        stmts = scale ((`div` 2) . intRoot) $ listOf $ arbitraryType >>= arbitraryExpression
        exp = scale intRoot $ arbitraryExpression t

arbitraryWhile :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe IncompleteExpression)
arbitraryWhile t = do
  isPure <- asks isPureEnv
  case t of
    T.Unit | not isPure -> Just <$> (While <$> invariant <*> subtree T.Boolean <*> (arbitraryType >>= subtree))
    _                   -> return Nothing
  where subtree t = scale (`div` 2) $ arbitraryExpression t
        invariant = local (envWithPureness True) (scale intRoot $ listOf $ subtree T.Boolean)

arbitraryLeafExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g IncompleteExpression
arbitraryLeafExpression t = do
  var <- arbitraryVariable t
  c <- arbitraryConstant t
  case var of
    Nothing -> return c
    Just v  -> elements [v, c]

arbitraryConstant :: MonadGen g => Type -> g IncompleteExpression
arbitraryConstant T.Boolean = arbitraryBoolean
arbitraryConstant T.Integer = arbitraryInteger
arbitraryConstant T.Double  = arbitraryDouble
arbitraryConstant T.Unit    = return S.Unit

arbitraryInnerExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g IncompleteExpression
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
  unreliableArbitraries <- catMaybes <$> sequence [arbitraryCall t, arbitraryWhile t, arbitraryAssertion t]
  let anyTyped = map ($ t) [arbitraryLeafExpression, arbitraryLeafExpression, arbitraryConditional, arbitraryBlock]
                 ++ map return unreliableArbitraries
  -- We want the same probability to get a unary operation, a binary operation, a constant, a variable, a call or a conditional
  let weighted = map ((,) weightOthers) anyTyped
                 ++ map ((,) weightBinOps . return) binOps
                 ++ map ((,) weightUnOps . return) unOps
  frequency weighted

arbitraryExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g TypeCheckerExpression
arbitraryExpression t = addExpMeta t $ sized expression'
  where expression' :: (MonadGen g, MonadReader GeneratorEnv g) => Int -> g IncompleteExpression
        expression' 0         = arbitraryLeafExpression t
        expression' n | n > 0 = arbitraryInnerExpression t
        expression' s         = error $ "expression' for negative size " ++ show s ++ " not supported."

-- | The most simple expression with a given type. Used for variable shrinking.
trivial :: TypMeta -> TypeCheckerExpression
trivial m = trivial' (typTyp m) m mkNodeMeta
  where trivial' T.Boolean = S.Boolean False
        trivial' T.Integer = S.Integer 0
        trivial' T.Double  = S.Double 0.0
        trivial' T.Unit    = S.Unit

-- | Returns the free variables in an expression. Used to determine which declarations are shrinkable.
freeVariables :: TypeCheckerExpression -> S.Set TypeCheckerTypedVariable
freeVariables = foldMapExpression freeVariable
  where freeVariable v@(Variable a _ _ _) = S.singleton $ TypedVariable a (expressionTyp' v) () mkNodeMeta
        freeVariable _                    = S.empty

-- | Returns the called functions in a program. Used to determine which signatures are shrinkable.
calledFunctions :: TypeCheckerProgram -> S.Set FunctionKey
calledFunctions = foldMapExpressions calledFunctionsExpression
  where calledFunctionsExpression c@Call{} = S.singleton $ callFunctionKey c
        calledFunctionsExpression _        = S.empty

shrinkExpression :: TypeCheckerExpression -> [TypeCheckerExpression]
shrinkExpression b@S.Boolean{ boolValue = val }          = [b{ boolValue = v } | v <- Q.shrink val]
shrinkExpression n@S.Integer{ intValue = val }           = [n{ intValue = v } | v <- Q.shrink val]
shrinkExpression d@S.Double{ doubleValue = val }         = [d{ doubleValue = v } | v <- Q.shrink val]
shrinkExpression S.Unit{}                                = []
shrinkExpression (Variable _ _ m _)                      = [trivial m]
shrinkExpression (BinaryOperation Assign left right m p) = childrenWithType m [left, right]
                                                           ++ [BinaryOperation Assign left r m p | r <- Q.shrink right]
shrinkExpression (BinaryOperation op left right m p)     = childrenWithType m [left, right]
                                                           ++ [BinaryOperation op l r m p | (l, r) <- Q.shrink (left, right)]
shrinkExpression (UnaryOperation op exp m p)             = childrenWithType m [exp]
                                                           ++ [UnaryOperation op e m p | e <- Q.shrink exp]
shrinkExpression (Call name args cm m p)                 = childrenWithType m args
                                                           ++ [Call name a cm m p | a <- shrinkArgs args]
  where shrinkArgs :: [TypeCheckerExpression] -> [[TypeCheckerExpression]]
        shrinkArgs []     = []
        shrinkArgs (x:xs) = [y : xs | y <- Q.shrink x] ++ [x : ys | ys <- shrinkArgs xs]
shrinkExpression (Conditional cond ifExp elseExp m p) = childrenWithType m [cond, ifExp, elseExp]
                                                        ++ [Conditional c i e m p | (c, i, e) <- Q.shrink (cond, ifExp, elseExp)]
shrinkExpression (Block stmts exp m p)                = [exp | S.null $ freeVariables exp `S.intersection` definedVars]
                                                        ++ [Block (init stmts) (last stmts) m p | not (null stmts), expressionTyp' (last stmts) == typTyp m]
                                                        ++ [Block s exp m p | s <- shrinkStmts stmts]
                                                        ++ [Block stmts e m p | e <- Q.shrink exp]
  where freeVars = foldMap freeVariables (exp : stmts)
        definedVars = foldMap definedVar (exp : stmts)
        definedVar (VarDef v _ _ _ _) = S.singleton v
        definedVar _                  = S.empty
        isRemovable (VarDef v _ _ _ _) = v `notElem` freeVars
        isRemovable _                  = True
        shrinkStmts :: [TypeCheckerExpression] -> [[TypeCheckerExpression]]
        shrinkStmts []     = []
        shrinkStmts (x:xs) = [xs | isRemovable x] ++ [y : xs | y <- Q.shrink x] ++ [x : ys | ys <- shrinkStmts xs]
shrinkExpression (While invs cond body m p)           = S.Unit m p
                                                        : childrenWithType m [body]
                                                        ++ [While i c b m p | (i, c, b) <- Q.shrink (invs, cond, body)]
shrinkExpression (Assertion k exp m p)                = S.Unit m p : [Assertion k e m p | e <- Q.shrink exp]
shrinkExpression (VarDef var isVal exp m p)           = [VarDef var isVal e m p | e <- Q.shrink exp]

childrenWithType :: TypMeta -> [TypeCheckerExpression] -> [TypeCheckerExpression]
childrenWithType m = filter (\c -> expressionTyp' c == (typTyp m))

shrinkSignature :: MonadReader (S.Set FunctionKey) m => S.Set ParserTypedVariable -> TypeCheckerSignature -> m [TypeCheckerSignature]
shrinkSignature free sig@Signature{..} = do
  functionNames <- asks $ S.map funcName
  isRemovable <- asks $ flip isRemovableSignature sig
  let shrinkedConds = [Signature isPure sigName args retType precs' posts' sigMeta sigNodeMeta |
                       (precs', posts') <- Q.shrink (preconditions, postconditions)]
  let shrinkedArgs = [Signature isPure sigName args' retType preconditions postconditions sigMeta sigNodeMeta |
                      args' <- shrinkArgs isRemovable args]
  shrinkedNameIdentifiers <- runReaderT (shrinkIdentifier sigName) functionNames
  let shrinkedNames = [Signature isPure sigName' args retType preconditions postconditions sigMeta sigNodeMeta |
                       sigName' <- shrinkedNameIdentifiers, isRemovable]
  return $ shrinkedConds ++ shrinkedArgs ++ shrinkedNames
  where shrinkArgs :: Bool -> [ParserTypedVariable] -> [[ParserTypedVariable]]
        shrinkArgs isRemovable args = shrinkArgs' isRemovable args $ S.fromList $ map varName args
        shrinkArgs' :: Bool -> [ParserTypedVariable] -> S.Set Name -> [[ParserTypedVariable]]
        shrinkArgs' _ [] _                                          = []
        shrinkArgs' isRemovable (x:xs) argNames | x `S.member` free = [x : ys | ys <- shrinkArgs' isRemovable xs argNames]
                                                | otherwise         = [xs | isRemovable]  -- We can only remove arguments if the function is never called.
                                                                      ++ [y : xs | y <- runReader (shrinkTypedVariable x) argNames]
                                                                      ++ [x : ys | ys <- shrinkArgs' isRemovable xs argNames]

isRemovableSignature :: S.Set FunctionKey -> TypeCheckerSignature -> Bool
isRemovableSignature funcs sig = functionKey sig `S.notMember` funcs

condFreeVariables :: TypeCheckerSignature -> S.Set TypeCheckerTypedVariable
condFreeVariables Signature{..} = freeVars preconditions `S.union` freeVars postconditions
  where freeVars = foldr S.union S.empty . map freeVariables

shrinkDeclaration :: MonadReader (S.Set FunctionKey) m => TypeCheckerDeclaration -> m [TypeCheckerDeclaration]
shrinkDeclaration (Function sig body meta) = do
  sigShrinks <- map (\s -> Extern s meta) <$> shrinkSignature free sig
  return $ sigShrinks ++ [Function sig b meta | b <- shrinkExpression body]
  where free = condFreeVariables sig `S.union` freeVariables body
shrinkDeclaration (Extern sig meta)        = do
  map (\s -> Extern s meta) <$> shrinkSignature free sig
  where free = condFreeVariables sig

shrinkProgram :: TypeCheckerProgram -> [TypeCheckerProgram]
shrinkProgram p = shrinkProgram' (calledFunctions p) p
  where meta = S.progMeta p
        shrinkProgram' _ (Program [] _)         = []
        shrinkProgram' funcs (Program (x:xs) _) = headRemovals ++ map appendTail headShrinks ++ map appendHead tailShrinks
          where headShrinks = runReader (shrinkDeclaration x) funcs
                tailShrinks = shrinkProgram' funcs $ Program xs meta
                headRemovals = [Program xs meta | isRemovableDeclaration funcs x]
                isRemovableDeclaration funcs = isRemovableSignature funcs . signature
                appendTail y = Program (y:xs) meta
                appendHead (Program ys p) = Program (x : ys) p

initialEnvWithPureness :: Bool -> GeneratorEnv
initialEnvWithPureness isPure = GeneratorEnv{ callables = M.empty, functions = M.empty, variables = M.empty, isPureEnv = isPure }

envWithPureness :: Bool -> GeneratorEnv -> GeneratorEnv
envWithPureness isPure env = env{ callables = callables', isPureEnv = isPure }
  where callables' = if isPure then functions env else callables env

newtype PureExpression
  = PureExpression { runPureExpression :: TypeCheckerExpression }
  deriving (Eq, Ord, Show)

instance Q.Arbitrary TypeCheckerExpression where
  arbitrary = do
    typ <- arbitraryType
    pure <- choose (False, True)
    runReaderT (arbitraryExpression typ) (initialEnvWithPureness pure)
  shrink = shrinkExpression

instance Q.Arbitrary PureExpression where
  arbitrary = do
    typ <- arbitraryType
    PureExpression <$> runReaderT (arbitraryExpression typ) (initialEnvWithPureness True)
  shrink = map PureExpression . shrinkExpression . runPureExpression

instance Q.Arbitrary TypeCheckerTypedVariable where
  arbitrary = evalStateT arbitraryTypedVariable S.empty
  shrink v = runReader (shrinkTypedVariable v) S.empty

instance Q.Arbitrary TypeCheckerSignature where
  arbitrary = arbitrarySignature
  shrink sig = runReader (shrinkSignature S.empty sig) S.empty

instance Q.Arbitrary TypeCheckerDeclaration where
  arbitrary = arbitraryDeclaration
  shrink decl = runReader (shrinkDeclaration decl) S.empty

instance Q.Arbitrary TypeCheckerProgram where
  arbitrary = arbitraryProgram
  shrink prog = shrinkProgram prog

instance Q.Arbitrary Type where
  arbitrary = arbitraryType

instance Q.Arbitrary UnaryOperator where
  arbitrary = elements unaryOperators

instance Q.Arbitrary BinaryOperator where
  arbitrary = elements binaryOperators
