{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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
import Sara.Symbolize
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
import Text.Parsec.Pos
import qualified Data.Map.Strict as Map

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

identifier :: MonadGen g => g Name
identifier = ident `suchThat` isNotReserved

-- | Tests whether the identifier is not a reserved name.
isNotReserved :: MonadState [Name] m => Name -> m Bool
isNotReserved a = do
  names <- get
  return a /= "main" && a `notElem` reservedNames && a `notElem` names

shrinkIdentifier :: Name -> [Name]
shrinkIdentifier = filter (\a -> not (null a) && isNotReserved a) . Q.shrink

testfile :: String
testfile = "<testfile>"

position :: SourcePos
position = newPos testfile 0 0

mkNodeMeta :: NodeMeta
mkNodeMeta = NodeMeta pos

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
                     , sigMeta :: (a, d) }

toSignature :: PartialSignature -> [TypeCheckerExpression] -> [TypeCheckerExpression] -> TypeCheckerSignature
toSignature PartialSignature{..} pres posts = Signature isPure sigName args retType pres posts sigMeta

-- | Environment for the generation of expressions. The methods and functions are keyed by return type.
data GeneratorEnv
  = GeneratorEnv { callables :: M.Map T.Type [PartialSignature] -- ^ In a pure context, these are the functions, otherwise, functions and methods.
                 , functions :: M.Map T.Type [PartialSignature] -- ^ Pure functions.
                 , variables :: M.Map T.Type [Name]
                 , isPure :: Bool }

initialEnv :: [PartialSignature] -> GeneratorEnv
initialEnve = GeneratorEnv { methods = keyBy S.retType sigs
                           , filter isPure methods
                           , variables = M.empty
                           , isPure = undefined }

arbitraryProgram :: MonadGen g => g TypeCheckerProgram
arbitraryProgram = addNodeMeta $ scale intRoot $ do
  sigs <- evalStateT (listOf arbitrarySignature) S.empty
  decls <- runReaderT (mapM arbitraryDeclForSignature sigs) (initialEnv sigs)

arbitraryPartialSignature :: (MonadGen g, MonadState (S.Set FunctionKey) g) => g PartialSignature
arbitraryPartialSignature = do
  sigs <- get
  sig <- arbitrarySignature' `suchThat` (\sig -> functionKey sig `notMember` sigs)
  modify $ S.insert $ functionKey sig
  return sig
  where arbitrarySignature' = do
          isPure <- arbitraryBoolean
          name <- arbitraryIdentifier
          args <- evalStateT (scale intRoot $ listOf $ arbitraryTypedVariable) S.empty
          retType <- arbitraryType
          return $ PartialSignature isPure name args retType mkNodePlusMeta

arbitraryTypedVariable :: (MonadGen g, MonadState [Name] g) => g TypeCheckerSignature
arbitraryTypedVariable = mkNodePlusMeta $ do
  names <- gets
  name <- arbitraryIdentifier `suchThat` flip notMember names
  modify $ S.insert name
  typ <- arbitraryType
  return $ TypedVariable name typ mkNodePlusMeta

arbitraryType :: MonadGen g => g Type
arbitraryType = elements [T.Unit, T.Boolean, T.Integer, T.Double]

-- | The transformation of the environment that a partial signature causes, i.e. the arguments are added to the local variables.
partialSigEnvTransform :: PartialSignature -> GeneratorEnv -> GeneratorEnv
partialSigEnvTransform sig env = env{ variables = variables', callables = callables', isPure = isPure sig }
  where args = map S.varName $ keyBy S.varType $ args sig
        variables' = unionWith (++) (variables env) args
        callables' = if isPure sig then callables env else functions env

arbitrarySignatureForPartialSig :: (MonadGen g, MonadReader GeneratorEnv g) => PartialSignature -> g TypeCheckerSignature
arbitrarySignatureForPartialSig partialSig = toSignature <$> conditions <*> conditions
  where conditions = local (partialSigEnvTransform sig) $ scale intRoot $ listOf $ arbitraryExpression T.Boolean

arbitrarySignature :: MonadGen g => g TypeCheckerSignature
arbitrarySignature = do
  partialSig <- evalStateT arbitraryPartialSignature S.empty
  runReaderT (arbitrarySignatureForPartialSig sig) (initialEnv [sig])

arbitraryDeclForSignature :: (MonadGen g, MonadReader GeneratorEnv g) => PartialSignature -> g TypeCheckerDeclaration
arbitraryDeclForSignature partialSig = do
  sig <- arbitrarySignatureForPartialSig partialSig
  let sig' = toSignature sig pres posts
  body <- local envTransform $ arbitraryExpression $ retType sig
  return $ Function sig' body mkNodeMeta

arbitraryDeclaration :: MonadGen g -> g TypeCheckerDeclaration
arbitraryDeclaration = do
  sig <- evalStateT arbitraryPartialSignature S.empty
  runReaderT (arbitraryDeclForSignature sig) (initialEnv [sig])

type ExpMeta = (ExpressionMeta, NodeMeta)
type UntypedExpression = ExpMeta -> TypeCheckerExpression

arbitraryBoolean :: MonadGen g => g UntypedExpression
arbitraryBoolean = S.Boolean <$> choose (False, True)

-- | Generates a positive integer expression.
-- We don't like negative numbers here because -4 gets parsed as 4 with the unary minus applied to it.
arbitraryInteger :: MonadGen g => g UntypedExpression
arbitraryInteger = S.Integer <$> arbitrarySizedNatural

arbitraryDouble :: MonadGen g => g UntypedExpression
arbitraryDouble = liftM S.Double $ elements niceDoubles
  where niceDoubles = [0.0, 0.1, 1.0, 1.1, 1e10, 1.1e10]

arbitraryVariable :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g Maybe UntypedExpression
arbitraryVariable t = do
  vars <- asks $ M.lookup t . variables
  case vars of
    Nothing   -> Nothing
    Just []   -> Nothing
    Just vars -> Variable <$> elements vars

arbitraryCall :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g (Maybe UntypedExpression)
arbitraryCall t = do
  calls <- asks $ M.lookup t . callables
  case vars of
    Nothing    -> []
    Just []    -> []
    Just calls -> do
      call <- elements calls
      let numArgs = length $ args call
      as <- scale (`div` numArgs) mapM (expression . varType) (args call)
      return $ [Call (sigName call) as ()]

intRoot :: Int -> Int
intRoot = round . sqrt . fromIntegral

invert :: (Ord k, Ord v) => M.Map k v -> M.Map v [k]
invert m = M.fromListWith (++) pairs
  where pairs = [(v, [k]) | (k, v) <- M.toList m]

invertedUnOps :: M.Map Type TypedUnOp
invertedUnOps = invert typedUnOps

invertedBinOps :: M.Map Type TypedBinOp
invertedBinOps = invert typedBinOps

findListWithDefault :: Ord k => M.Map k [v] -> k -> [v]
findListWithDefault map key = M.findWithDefault [] key map

typUnOps :: Type -> [TypedUnOp]
typUnOps = findListWithDefault invertedUnOps
          
typBinOps :: Bool -> Type -> [TypedBinOp]
typBinOps = findListWithDefault invertedUnOps
typBinOps pure = filter (\(TypedBinOp op _ _) -> not pure || op /= Assign) . findListWithDefault invertedBinOps

arbitraryAssignable t :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g TypeCheckerExpression
arbitraryAssignable t = addExpMeta t $ variable t

-- | Creates one generator for each possible binary operator for that type.
binaryOperations :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> [g UntypedExpression]
binaryOperations t = do
  pure <- asks isPure
  map binOp $ typBinOps pure t
  where binOp :: (MonadGen g, MonadReader GeneratorEnv g) => TypedBinOp -> g UntypedExpression
        binOp (TypedBinOp Assign r s) = BinaryOperation Assign <$> arbitraryAssignable r <*> expression s
        binOp (TypedBinOp op r s)     = BinaryOperation op <$> subtree r <*> subtree s
        subtree r = scale (`div` 2) $ expression r
                                 
-- | Creates one generator for each possible unary operator for that type.
unaryOperations :: (MonadGen g, MonadReader GeneratorEnv g) => Bool -> Type -> [g UntypedExpression]
unaryOperations t = map unOp $ typUnOps t
  where unOp (TypedUnOp op s) = UnaryOperation op <$> subtree s
        subtree s = scale pred $ expression s

conditional :: (MonadGen g, MonadReader GeneratorEnv g) => Bool -> Type -> g UntypedExpression
conditional t = Conditional <$> subtree T.Boolean <*> subtree t <*> subtree t
  where subtree t = scale (`div` 3) $ expression t

block :: (MonadGen g, MonadReader GeneratorEnv g) => Bool -> Type -> g UntypedExpression
block t = Block <$> stmts <*> exp
  where stmts = scale intRoot $ listOf $ arbitraryType >>= expression pure
        exp = scale intRoot $ expression t

while :: (MonadGen g, MonadReader GeneratorEnv g) => g (Maybe UntypedExpression)
while t = do
  pure <- asks isPure
  if t == T.Unit and isPure then
    Just . While <$> subtree T.Boolean <*> (arbitraryType >>= subtree)
  else
    return Nothing
  where subtree t = scale (`div` 2) $ expression t

leafExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g UntypedExpression
leafExpression t = oneof [arbitraryConstant t, arbitraryVariable t]

arbitraryConstant :: MonadGen g => Type -> g UntypedExpression
arbitraryConstant T.Boolean = arbitraryBoolean
arbitraryConstant T.Integer = arbitraryInteger
arbitraryConstant T.Double  = arbitraryDouble
arbitraryConstant T.Unit    = return S.Unit

innerExpression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g UntypedExpression
innerExpression t =
  frequency weighted
  where weighted = map ((,) weightOthers) anyTyped ++ map ((,) weightBinOps) binOps ++ map ((,) weightUnOps) unOps
        anyTyped = map ($ t) $ [leafExpression, leafExpression, conditional, block] ++ catMaybes [call, while]
        binOps = binaryOperations t
        unOps = unaryOperations t
        numUnOps = length unOps
        numBinOps = length binOps
        -- We want the same probability to get a unary operation, a binary operation, a constant, a variable, a call or a conditional
        weightOthers = if numUnOps == 0 then weightUnOps else weightUnOps * weightBinOps
        weightUnOps = if numBinOps == 0 then 1 else numBinOps
        weightBinOps = if numUnOps == 0 then 1 else numUnOps

expression :: (MonadGen g, MonadReader GeneratorEnv g) => Type -> g TypeCheckerExpression
expression t = addExpMeta t $ sized expression'
  where expression' :: (MonadGen g, MonadReader GeneratorEnv g) => Int -> g UntypedExpression
        expression' 0         = leafExpression t
        expression' n | n > 0 = innerExpression t
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
  where freeVariable :: TypeCheckerExpression -> [ParserTypedVariable]
        freeVariable v@(Variable a _ _) = S.singleton $ TypedVariable a (expressionTyp v) ((), mkNodeMeta)
        freeVariable _                  = S.empty

-- | Returns the called functions in a program. Used to determine which signatures are shrinkable.
calledFunctions :: TypeCheckerProgram -> S.Set FunctionKey
calledFunctions = foldMapExpressions calledFunctionsExpression
  where calledFunctionsExpression :: TypeCheckerExpression -> [FunctionType]
        calledFunctionsExpression c@Call{} = S.singleton $ callFunctionKey c
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

shrinkTypedVariable :: TypeCheckerTypedVariable -> [TypeCheckerTypedVariable]
shrinkTypedVariable (TypedVariable var typ p) = [TypedVariable v typ p | v <- shrinkIdentifier var]

shrinkSignature :: Bool -> [ParserTypedVariable] -> TypeCheckerSignature -> [TypeCheckerSignature]
shrinkSignature isRemovable free (Signature pure name args typ precs posts p) =
  [Signature pure name args typ precs' posts' p | (precs', posts') <- Q.shrink (precs, posts)]
  ++ [Signature pure name a typ precs posts p | a <- shrinkArgTypes args]
  ++ [Signature pure n a typ precs posts p | n <- shrinkIdentifier name, isRemovable]
  where shrinkArgTypes :: [ParserTypedVariable] -> [[ParserTypedVariable]]
        shrinkArgTypes []                       = []
        shrinkArgTypes (x:xs) | x `member` free = [x : ys | ys <- shrinkArgTypes xs]
                              | otherwise       = [xs | isRemovable]  -- We can only remove arguments if the function is never called.
                                                  ++ [y : xs | y <- shrinkTypedVariable x]
                                                  ++ [x : ys | ys <- shrinkArgTypes xs]

isRemovableSignature :: [FunctionType] -> TypeCheckerSignature -> Bool
isRemovableSignature sig = functionKey sig `notElem` funcs

shrinkDeclaration :: [FunctionType] -> TypeCheckerDeclaration -> [TypeCheckerDeclaration]
shrinkDeclaration funcs (Function sig body meta) = let
  isRemovable = isRemovableSignature sig
  free = freeVariables body
  in [Function s body meta | s <- shrinkSignature isRemovable free]
     ++ [Function sig b meta | b <- shrinkExpression body]
shrinkDeclaration funcs (Extern sig meta) = let
  isRemovable = isRemovableSignature sig
  free = freeVariables body
  in [Function s meta | s <- shrinkSignature isRemovable free]

shrinkProgram :: TypeCheckerProgram -> [TypeCheckerProgram]
shrinkProgram p = shrinkProgram' (calledFunctions p) p
  where meta = S.progMeta p
        shrinkProgram' _ (Program [] _)         = []
        shrinkProgram' funcs (Program (x:xs) _) = headRemovals ++ map appendTail headShrinks ++ map appendHead tailShrinks
          where headShrinks = shrinkDeclaration funcs x
                tailShrinks = shrinkProgram' funcs $ Program xs meta
                headRemovals = if isRemovableDeclaration funcs x then Program xs meta else []
                isRemovableDeclaration funcs = isRemovableSignature funcs . signature
                shrinkSig (Function sig body p) = [Function s body p | s <- shrinkSignature (freeVariables body) sig]
                shrinkSig (Extern sig p)        = [Extern s p | s <- shrinkSignature [] sig]
                appendTail y = Program (y:xs) meta
                appendHead (Program ys p) = Program (x : ys) p

emptyEnvWithPureness :: Bool -> GeneratorEnv
emptyEnvWithPureness isPure' = GeneratorEnv{ callables = M.empty, functions = M.empty, variables = M.empty, isPure = isPure' }

newtype PureExpression
  = PureExpression { runPureExpression :: TypeCheckerExpression }
  deriving (Eq, Ord, Show)

instance Q.Arbitrary TypeCheckerExpression where
  arbitrary = do
    typ <- arbitraryType
    pure <- choose (False, True)
    runStateT arbitraryExpression (initialEnvWithPureness pure)
  shrink = shrinkExpression

instance Q.Arbitrary PureExpression where
  arbitrary = arbitraryType >>= PureExpression <$> runStateT arbitraryExpression (initialEnvWithPureness True)
  shrink = map PureExpression . shrinkExpression . runPureExpression

instance Q.Arbitrary TypeCheckerTypedVariable where
  arbitrary = runStateT arbitraryTypedVariable S.empty
  shrink = shrinkTypedVariable

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
