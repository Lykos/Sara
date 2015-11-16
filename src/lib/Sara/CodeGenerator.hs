{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Sara.CodeGenerator ( codegen
                          , PredeterminedForValue(..)
                          , ValueWhenPredetermined(..)
                          , ValueWhenNotPredetermined(..)
                          , withModule ) where

import qualified Sara.Syntax as S
import qualified Sara.Types as T
import Sara.Operators
import Sara.Meta

import Data.Word
import Data.List
import Data.Function
import qualified Data.Map.Strict as M

import Control.Monad.State.Strict

import LLVM.General.AST
import LLVM.General.AST.Global

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP

type SymbolTable = M.Map VariableMeta Operand

data CodegenState
  = CodegenState {
    currentBlock :: Name                   -- Name of the active block to append to
  , blocks       :: M.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable            -- Function scope symbol table
  , blockCount   :: Int                    -- Count of basic blocks
  , count        :: Word                   -- Count of unnamed instructions
  , names        :: Names                  -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: MonadState Module m => Definition -> m ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ M.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) M.empty M.empty 1 0 M.empty

execCodegen :: State CodegenState a -> CodegenState
execCodegen m = execState m emptyCodegen

addBlock :: MonadState CodegenState m => String -> m Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = M.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply }
  return (Name qname)

setBlock :: MonadState CodegenState m => Name -> m Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: MonadState CodegenState m => m Name
getBlock = gets currentBlock

modifyBlock :: MonadState CodegenState m => BlockState -> m ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = M.insert active new (blocks s) }

current :: MonadState CodegenState m => m BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case M.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

newTmpName :: MonadState CodegenState m => m Word
newTmpName = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

type Names = M.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns = case M.lookup nm ns of
  Nothing -> (nm,  M.insert nm 1 ns)
  Just ix -> (nm ++ show ix, M.insert nm (ix+1) ns)

booleanBits :: Word32
booleanBits = 1

integerBits :: Word32
integerBits = 64

typ :: T.Type -> Type
typ T.Unit    = IntegerType booleanBits
typ T.Boolean = IntegerType booleanBits
typ T.Integer = IntegerType integerBits
typ T.Double  = FloatingPointType 64 IEEE

booleanType :: Type
booleanType = typ T.Boolean

define :: MonadState Module m => PureCheckerSignature -> [BasicBlock] -> m ()
define S.Signature{ S.sigName = label, S.args = args, S.retType = retty } body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter (typ ty) (varMetaName m) [] | (S.TypedVariable _ ty m _) <- args], False)
  , returnType  = typ retty
  , basicBlocks = body
  }

extern :: MonadState Module m => PureCheckerSignature -> m ()
extern = flip define []

local :: Name -> Type -> Operand
local = flip LocalReference

assign :: MonadState CodegenState m => VariableMeta -> Operand -> m ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s{ symtab = M.insert var x lcls }

getVar :: MonadState CodegenState m => VariableMeta -> m Operand
getVar var = do
  syms <- gets symtab
  case M.lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: MonadState CodegenState m => Instruction -> Type -> m Operand
instr ins t = do
  n   <- newTmpName
  blk <- current
  let i = stack blk
  let ref = UnName n
  modifyBlock $ blk { stack = i ++ [ref := ins] }
  return $ local ref t

terminator :: MonadState CodegenState m => Named Terminator -> m (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock $ blk { term = Just trm }
  return trm

br :: MonadState CodegenState m => Name -> m (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: MonadState CodegenState m => Operand -> Name -> Name -> m (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: MonadState CodegenState m => [(Operand, Name)] -> Type -> m Operand
phi incoming ty = (instr $ Phi ty incoming []) ty

ret :: MonadState CodegenState m => Operand -> m (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: MonadState CodegenState m => S.Name -> [Operand] -> Type -> m Operand
call name args retType = (instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []) retType
  where fn = ConstantOperand $ C.GlobalReference retType $ Name name
        toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
        toArgs = map (\x -> (x, []))

alloca :: MonadState CodegenState m => Type -> m Operand
alloca ty = (instr $ Alloca ty Nothing 0 []) ty

store :: MonadState CodegenState m => Operand -> Operand -> Type -> m Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: MonadState CodegenState m => Operand -> Type -> m Operand
load ptr = instr $ Load False ptr Nothing 0 []

codegenDeclaration :: MonadState Module m => PureCheckerDeclaration -> m ()
codegenDeclaration (S.Extern sig _)        = extern sig
codegenDeclaration (S.Function sig body _) = do
  f <- codegenFunction sig body
  return f

codegenFunction :: MonadState Module m => PureCheckerSignature -> PureCheckerExpression -> m ()
codegenFunction signature body = define signature bls
  where
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ (S.args signature) $ \arg -> do
        let meta = S.varMeta arg
        let t = typ $ varSymType meta
        initializeVar meta $ local (varMetaName meta) t
      codegenExpression body >>= ret

initializeVar :: MonadState CodegenState m => VariableMeta -> Operand -> m ()
initializeVar meta o = do
  let t = typ $ varSymType meta
  var <- alloca t
  store var o t
  assign meta var

varMetaName :: VariableMeta -> Name
varMetaName (VariableMeta _ name index) = Name $ name ++ show index
varMetaName (BuiltinVar _ e)            = error $ "No code generation for builtin variable " ++ show e ++ " supported."

codegenProgram :: MonadState Module m => PureCheckerProgram -> m ()
codegenProgram (S.Program p _) = mapM_ codegenDeclaration p

codegen :: Module -> PureCheckerProgram -> Module
codegen mod program = execState (codegenProgram program) mod

withModule :: String -> (Module -> a) -> a
withModule name f = f $ emptyModule name

cons :: C.Constant -> Operand
cons = ConstantOperand

booleanToInteger :: Bool -> Integer
booleanToInteger True  = 1
booleanToInteger False = 0

unit :: MonadState CodegenState m => m Operand
unit = true1

boolean :: MonadState CodegenState m => Bool -> m Operand
boolean b = return $ cons $ C.Int booleanBits $ booleanToInteger b

true1 :: MonadState CodegenState m => m Operand
true1 = boolean True

integer :: MonadState CodegenState m => Integer -> m Operand
integer n = return $ cons $ C.Int integerBits n

true64 :: MonadState CodegenState m => m Operand
true64 = integer 0xFFFFFFFFFFFFFFFF

integerZero :: MonadState CodegenState m => m Operand
integerZero = integer 0

double :: MonadState CodegenState m => Double -> m Operand
double d = return $ cons $ C.Float $ F.Double d

doubleZero :: MonadState CodegenState m => m Operand
doubleZero = double 0

logicalNot :: MonadState CodegenState m => Operand -> m Operand
logicalNot a = do
  true <- true1
  (instr $ Xor true a []) booleanType

unaryInstruction :: MonadState CodegenState m => T.TypedUnOp -> Operand -> Type -> m Operand
unaryInstruction (T.TypedUnOp UnaryPlus T.Integer) a _  = return a
unaryInstruction (T.TypedUnOp UnaryPlus T.Double) a _   = return a
unaryInstruction (T.TypedUnOp UnaryMinus T.Integer) a t = do
  zero <- integerZero
  (instr $ Sub False False zero a []) t
unaryInstruction (T.TypedUnOp UnaryMinus T.Double) a t  = do
  zero <- doubleZero
  (instr $ FSub NoFastMathFlags zero a []) t
unaryInstruction (T.TypedUnOp BitwiseNot T.Integer) a t = do
  true <- true64
  (instr $ Xor true a []) t
unaryInstruction (T.TypedUnOp LogicalNot T.Boolean) a _ = logicalNot a
unaryInstruction unop _ _                               = error $ "Unsupported unary operation " ++ show unop ++ "."

binaryInstruction :: MonadState CodegenState m => T.TypedBinOp -> Operand -> Operand -> Type -> m Operand
binaryInstruction (T.TypedBinOp LeftShift T.Integer T.Integer) a b =
  instr $ Shl False False a b []
binaryInstruction (T.TypedBinOp RightShift T.Integer T.Integer) a b =
  instr $ AShr False a b []
binaryInstruction (T.TypedBinOp BitwiseAnd T.Integer T.Integer) a b =
  instr $ And a b []
binaryInstruction (T.TypedBinOp BitwiseXor T.Integer T.Integer) a b =
  instr $ Xor a b []
binaryInstruction (T.TypedBinOp BitwiseOr T.Integer T.Integer) a b =
  instr $ Or a b []
binaryInstruction (T.TypedBinOp Times T.Integer T.Integer) a b =
  instr $ Mul False False a b []
binaryInstruction (T.TypedBinOp Times T.Double T.Double) a b =
  instr $ FMul NoFastMathFlags a b []
binaryInstruction (T.TypedBinOp DividedBy T.Integer T.Integer) a b =
  instr $ SDiv False a b []
binaryInstruction (T.TypedBinOp DividedBy T.Double T.Double) a b =
  instr $ FDiv NoFastMathFlags a b []
binaryInstruction (T.TypedBinOp Modulo T.Integer T.Integer) a b =
  instr $ SRem a b []
binaryInstruction (T.TypedBinOp Modulo T.Double T.Double) a b =
  instr $ FRem NoFastMathFlags a b []
binaryInstruction (T.TypedBinOp Plus T.Integer T.Integer) a b =
  instr $ Add False False a b []
binaryInstruction (T.TypedBinOp Plus T.Double T.Double) a b =
  instr $ FAdd NoFastMathFlags a b []
binaryInstruction (T.TypedBinOp Minus T.Integer T.Integer) a b =
  instr $ Sub False False a b []
binaryInstruction (T.TypedBinOp Minus T.Double T.Double) a b =
  instr $ FSub NoFastMathFlags a b []
binaryInstruction (T.TypedBinOp LessThan T.Integer T.Integer) a b =
  instr $ ICmp IP.SLT a b []
binaryInstruction (T.TypedBinOp LessThan T.Double T.Double) a b =
  instr $ FCmp FP.OLT a b []
binaryInstruction (T.TypedBinOp AtMost T.Integer T.Integer) a b =
  instr $ ICmp IP.SLE a b []
binaryInstruction (T.TypedBinOp AtMost T.Double T.Double) a b =
  instr $ FCmp FP.OLE a b []
binaryInstruction (T.TypedBinOp GreaterThan T.Integer T.Integer) a b =
  instr $ ICmp IP.SGT a b []
binaryInstruction (T.TypedBinOp GreaterThan T.Double T.Double) a b =
  instr $ FCmp FP.OGT a b []
binaryInstruction (T.TypedBinOp AtLeast T.Integer T.Integer) a b =
  instr $ ICmp IP.SGE a b []
binaryInstruction (T.TypedBinOp AtLeast T.Double T.Double) a b =
  instr $ FCmp FP.OGE a b []
binaryInstruction (T.TypedBinOp EqualTo T.Integer T.Integer) a b =
  instr $ ICmp IP.EQ a b []
binaryInstruction (T.TypedBinOp EqualTo T.Double T.Double) a b =
  instr $ FCmp FP.OEQ a b []
binaryInstruction (T.TypedBinOp NotEqualTo T.Integer T.Integer) a b =
  instr $ ICmp IP.NE a b []
binaryInstruction (T.TypedBinOp NotEqualTo T.Double T.Double) a b =
  instr $ FCmp FP.ONE a b []
binaryInstruction (T.TypedBinOp LogicalXor T.Boolean T.Boolean) a b =
  instr $ Xor a b []
binaryInstruction (T.TypedBinOp EquivalentTo T.Boolean T.Boolean) a b =
  instr $ ICmp IP.EQ a b []
binaryInstruction (T.TypedBinOp NotEquivalentTo T.Boolean T.Boolean) a b =
  instr $ ICmp IP.NE a b []
binaryInstruction binop _ _ = error $ "Unsupported binary operation " ++ show binop ++ "."

shortCircuit :: MonadState CodegenState m => String -> ShortCircuitKind -> PureCheckerExpression -> PureCheckerExpression -> m Operand
shortCircuit name ShortCircuitKind{..} left right = do
  rightBlock <- addBlock $ name ++ ".right"
  exitBlock <- addBlock $ name ++ ".exit"

  left' <- codegenExpression left
  return left'
  
  leftResult <- case valueWhenPredetermined of
    LeftSideWhenPredetermined    -> return left'
    NotLeftSideWhenPredetermined -> logicalNot left'
  case predeterminedForValue of
    PredeterminedForFalse -> cbr left' rightBlock exitBlock
    PredeterminedForTrue  -> cbr left' exitBlock rightBlock
  leftBlock <- getBlock

  setBlock rightBlock
  right' <- codegenExpression right
  rightResult <- case valueWhenNotPredetermined of
    RightSideWhenNotPredetermined    -> return right'
    NotRightSideWhenNotPredetermined -> logicalNot right'
  br exitBlock
  rightBlock <- getBlock

  setBlock exitBlock
  phi [(leftResult, leftBlock), (rightResult, rightBlock)] booleanType

codegenExpression :: MonadState CodegenState m => PureCheckerExpression -> m Operand
codegenExpression exp = let t' = typ $ expressionTyp exp in case exp of
  S.Unit{}                               -> unit
  (S.Boolean b _ _)                      -> boolean b
  (S.Integer n _ _)                      -> integer n
  (S.Double d _ _)                       -> double d
  (S.UnaryOperation op exp _ _)          -> do
    let op' = unaryInstruction (T.TypedUnOp op (expressionTyp exp))
    exp' <- codegenExpression exp
    op' exp' t'
  (S.BinaryOperation Assign var val _ _) -> do
    let m = case var of
          (S.Variable _ m _ _) -> m
          _                       -> error $ "Unsupported assignment lhs " ++ show var ++ "."
    var' <- getVar m
    val' <- codegenExpression val
    store var' val' t'
  (S.BinaryOperation op@(shortCircuitKind -> Just kind) left right _ _) -> shortCircuit (show op) kind left right
  (S.BinaryOperation op left right _ _)  -> do
    let op' = binaryInstruction (T.TypedBinOp op (expressionTyp left) (expressionTyp right))
    left' <- codegenExpression left
    right' <- codegenExpression right
    op' left' right' t'
  (S.Variable _ m _ _)                   -> do
    var' <- getVar m
    load var' t'
  (S.Call name args _ _ _)               -> do
    args' <- mapM codegenExpression args
    call name args' t'
  (S.Conditional cond ifExp elseExp _ _) -> do
    ifBlock <- addBlock "if.then"
    elseBlock <- addBlock "if.else"
    exitBlock <- addBlock "if.exit"
  
    cond' <- codegenExpression cond
    cbr cond' ifBlock elseBlock           -- Branch based on the condition

    setBlock ifBlock
    ifVal <- codegenExpression ifExp      -- Generate code for the true branch
    br exitBlock                          -- Branch to the merge block
    ifBlock <- getBlock

    setBlock elseBlock
    elseVal <- codegenExpression elseExp  -- Generate code for the false branch
    br exitBlock                          -- Branch to the merge block
    elseBlock <- getBlock

    setBlock exitBlock
    phi [(ifVal, ifBlock), (elseVal, elseBlock)] t'
  (S.Block stmts exp _ _)                -> do
    mapM codegenExpression stmts
    codegenExpression exp
  (S.While _ cond body _ _)              -> do
    whileBlock <- addBlock "while.body"
    exitBlock <- addBlock "while.exit"
    
    cond' <- codegenExpression cond
    cbr cond' whileBlock exitBlock

    setBlock whileBlock
    codegenExpression body
    cond'' <- codegenExpression cond
    cbr cond'' whileBlock exitBlock

    setBlock exitBlock
    unit
  S.Assertion{}                          -> unit
  (S.VarDef tVar _ exp _ _)              -> do
    exp' <- codegenExpression exp
    initializeVar (S.varMeta tVar) exp'
    unit
