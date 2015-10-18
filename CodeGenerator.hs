{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGenerator (
  codegen
  , withModule) where

import qualified Syntax as S
import qualified Types as T
import Operators

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import System.IO

import LLVM.General.AST
import LLVM.General.AST.Global

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)
  
newtype LLVM a = LLVM { unLLVM :: State Module a }
  deriving (Functor, Applicative, Monad, MonadState Module)

runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

newTmpName :: Codegen Word
newTmpName = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns = case Map.lookup nm ns of
  Nothing -> (nm,  Map.insert nm 1 ns)
  Just ix -> uniqueName' nm ns ix
    where uniqueName' :: String -> Names -> Int -> (String, Names)
          uniqueName' nm ns ix = case Map.lookup nm ns of
            Nothing -> (nm ++ show ix, Map.insert nm (ix + 1) ns)
            Just ix -> uniqueName' nm ns (ix + 1)

instance IsString Name where
  fromString = Name . fromString

booleanBits :: Word32
booleanBits = 1

integerBits :: Word32
integerBits = 64

typ :: T.Type -> Type
typ T.Unit    = IntegerType booleanBits
typ T.Boolean = IntegerType booleanBits
typ T.Integer = IntegerType integerBits
typ T.Double  = FloatingPointType 64 IEEE

define :: S.Signature -> [BasicBlock] -> LLVM ()
define (S.Signature label args retty) body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter (typ ty) (Name nm) [] | (S.TypedVariable nm ty _) <- args], False)
  , returnType  = typ retty
  , basicBlocks = body
  }

extern :: S.Signature -> LLVM ()
extern = flip define $ []

local :: Name ->  Type -> Operand
local = flip LocalReference

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getVar :: String -> Codegen Operand
getVar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Instruction -> Type -> Codegen Operand
instr ins t = do
  n   <- newTmpName
  blk <- current
  let i = stack blk
  let ref = (UnName n)
  modifyBlock $ blk { stack = i ++ [ref := ins] }
  return $ local ref t

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock $ blk { term = Just trm }
  return trm

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: [(Operand, Name)] -> Type -> Codegen Operand
phi incoming ty = (instr $ Phi ty incoming []) ty

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: S.Name -> [Operand] -> Type -> Codegen Operand
call name args retType = (instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []) retType
  where fn = ConstantOperand $ C.GlobalReference retType $ Name name
        toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
        toArgs = map (\x -> (x, []))

alloca :: Type -> Codegen Operand
alloca ty = (instr $ Alloca ty Nothing 0 []) ty

store :: Operand -> Operand -> Type -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Type -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

codegenDeclarationAst :: S.DeclarationAst -> LLVM ()
codegenDeclarationAst = codegenDeclaration . S.decl

codegenDeclaration :: S.Declaration -> LLVM ()
codegenDeclaration (S.Extern sig)        = extern sig
codegenDeclaration (S.Function sig body) = codegenFunctionOrMethod sig body
codegenDeclaration (S.Method sig body)   = codegenFunctionOrMethod sig body

codegenFunctionOrMethod :: S.Signature -> S.ExpressionAst -> LLVM ()
codegenFunctionOrMethod signature body = define signature bls
  where
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM (S.args signature) $ \arg -> do
        let name = S.varName arg
        let t = typ $ S.varType arg
        var <- alloca t
        store var (local (Name name) t) t
        assign name var
      codegenExpressionAst body >>= ret

codegenProgram :: S.Program -> LLVM ()
codegenProgram (S.Program p) = (sequence $ map codegenDeclarationAst p) >> return ()

codegen :: Module -> S.Program -> Module
codegen mod program = runLLVM mod $ codegenProgram program

withModule :: String -> (Module -> a) -> a
withModule name f = f $ emptyModule name

cons :: C.Constant -> Operand
cons = ConstantOperand

booleanToInteger :: Bool -> Integer
booleanToInteger True  = 1
booleanToInteger False = 0

unit :: Codegen Operand
unit = true1

boolean :: Bool -> Codegen Operand
boolean b = return $ cons $ C.Int booleanBits $ booleanToInteger b

true1 :: Codegen Operand
true1 = boolean True

integer :: Integer -> Codegen Operand
integer n = return $ cons $ C.Int integerBits n

true64 :: Codegen Operand
true64 = integer 0xFFFFFFFFFFFFFFFF

integerZero :: Codegen Operand
integerZero = integer 0

double :: Double -> Codegen Operand
double d = return $ cons $ C.Float $ F.Double d

doubleZero :: Codegen Operand
doubleZero = double 0

unaryInstruction :: T.TypedUnOp -> Operand -> Type -> Codegen Operand
unaryInstruction (T.TypedUnOp UnaryPlus T.Integer) a t  = return a
unaryInstruction (T.TypedUnOp UnaryPlus T.Double) a t   = return a
unaryInstruction (T.TypedUnOp UnaryMinus T.Integer) a t = do
  zero <- integerZero
  (instr $ Sub False False zero a []) t
unaryInstruction (T.TypedUnOp UnaryMinus T.Double) a t  = do
  zero <- doubleZero
  (instr $ FSub NoFastMathFlags zero a []) t
unaryInstruction (T.TypedUnOp BitwiseNot T.Integer) a t = do
  true <- true64
  (instr $ Xor true a []) t
unaryInstruction (T.TypedUnOp LogicalNot T.Boolean) a t = do
  true <- true1
  (instr $ Xor true a []) t

binaryInstruction :: T.TypedBinOp -> Operand -> Operand -> Type -> Codegen Operand
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
binaryInstruction (T.TypedBinOp LogicalAnd T.Boolean T.Boolean) a b =
  instr $ And a b []
binaryInstruction (T.TypedBinOp LogicalXor T.Boolean T.Boolean) a b =
  instr $ Xor a b []
binaryInstruction (T.TypedBinOp LogicalOr T.Boolean T.Boolean) a b =
  instr $ Or a b []
binaryInstruction (T.TypedBinOp Implies T.Boolean T.Boolean) a b =
  instr $ ICmp IP.ULE a b []
binaryInstruction (T.TypedBinOp ImpliedBy T.Boolean T.Boolean) a b =
  instr $ ICmp IP.UGE a b []
binaryInstruction (T.TypedBinOp EquivalentTo T.Boolean T.Boolean) a b =
  instr $ ICmp IP.EQ a b []
binaryInstruction (T.TypedBinOp NotEquivalentTo T.Boolean T.Boolean) a b =
  instr $ ICmp IP.NE a b []

codegenExpressionAst :: S.ExpressionAst -> Codegen Operand
codegenExpressionAst (S.ExpressionAst exp t _) = let t' = typ t in case exp of
  S.Unit                             -> unit
  (S.Boolean b)                      -> boolean b
  (S.Integer n)                      -> integer n
  (S.Double d)                       -> double d
  (S.UnaryOperation op exp)          -> do
    let op' = unaryInstruction (T.TypedUnOp op (S.expType exp))
    exp' <- codegenExpressionAst exp
    op' exp' t'
  (S.BinaryOperation op left right)  -> do
    let op' = binaryInstruction (T.TypedBinOp op (S.expType left) (S.expType right))
    left' <- codegenExpressionAst left
    right' <- codegenExpressionAst right
    op' left' right' t'
  (S.Variable name)                  -> do
    var' <- getVar name
    load var' t'
  (S.Call name args)                 -> do
    args' <- mapM codegenExpressionAst args
    call name args' t'
  (S.Conditional cond ifExp elseExp) -> do
    ifBlock <- addBlock "if.then"
    elseBlock <- addBlock "if.else"
    exitBlock <- addBlock "if.exit"
  
    cond' <- codegenExpressionAst cond
    cbr cond' ifBlock elseBlock               -- Branch based on the condition

    setBlock ifBlock
    ifVal <- codegenExpressionAst ifExp      -- Generate code for the true branch
    br exitBlock                             -- Branch to the merge block
    ifBlock <- getBlock

    setBlock elseBlock
    elseVal <- codegenExpressionAst elseExp  -- Generate code for the false branch
    br exitBlock                             -- Branch to the merge block
    elseBlock <- getBlock

    setBlock exitBlock
    phi [(ifVal, ifBlock), (elseVal, elseBlock)] t'
  (S.Block stmts exp)                -> do
    stmts' <- mapM codegenExpressionAst stmts
    codegenExpressionAst exp
