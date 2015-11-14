module Sara.Z3.CodeGenerator where

import Z3.Monad
import Sara.Z3.Ast
import Sara.Z3.Utils
import Sara.Meta
import Sara.Utils
import Sara.Types

codegen :: MonadZ3 m => Ast -> m AST
codegen (BoolConst b)    = mkBool b
codegen (IntConst n)     = mkInteger n
codegen (Var a)          = z3Var a
codegen (App a bs)       = do
  func <- z3FuncDecl a
  args <- mapM codegen bs
  mkApp func args
codegen (UnOp op a)      = z3UnOp op =<< codegen a
codegen (BinOp op a b)   = do
  left <- codegen a
  right <- codegen b
  z3BinOp op left right
codegen (NaryOp op bs)   = z3NaryOp op =<< mapM codegen bs
codegen (Ite a b c)      = do
  cond <- codegen a
  thenExp <- codegen b
  elseExp <- codegen c
  mkIte cond thenExp elseExp
codegen (Forall as b cs) = do
  vars <- mapM z3Var as
  varApps <- mapM toApp vars
  patterns <- mapM codegenPattern cs
  ast <- codegen b
  mkForallConst patterns varApps ast
  where codegenPattern (Pattern ps) = do
          exps <- mapM codegen ps
          mkPattern exps

-- | Translates a type to a Z3 sort.
z3Sort :: MonadZ3 z3 => Type -> z3 Sort
z3Sort Boolean = mkBoolSort
z3Sort Integer = mkIntSort
z3Sort t       = error $ "Unsupported type for verifier: " ++ show t

z3VarSymbol :: MonadZ3 z3 => VariableMeta -> z3 Symbol
z3VarSymbol = mkStringSymbol . z3VarName

z3Var :: MonadZ3 z3 => VariableMeta -> z3 AST
z3Var m@(VariableMeta typ _ _) = do
  sym <- z3VarSymbol m
  sort <- z3Sort typ
  mkVar sym sort

z3FuncDecl :: MonadZ3 m => AppMeta -> m FuncDecl
z3FuncDecl m@(AppMeta _ (FunctionMeta isPure argTypes retType _ _)) = do
  funcSym <- mkStringSymbol $ z3FuncName m
  argSorts <- mapM z3Sort argTypes
  retSort <- z3Sort retType
  case isPure of
    True  -> mkFuncDecl funcSym argSorts retSort
    False -> return $ error "Non-pure functions cannot be called in Z3."

mkZero :: MonadZ3 z3 => z3 AST
mkZero = mkInteger 0

z3UnOp :: MonadZ3 z3 => UnaryOperator -> AST -> z3 AST
z3UnOp UnMinus e = do
  z <- mkZero
  mkSub [z, e]
z3UnOp Not e     = mkNot e

z3BinOp :: MonadZ3 z3 => BinaryOperator -> AST -> AST -> z3 AST
z3BinOp DividedBy       = mkDiv
z3BinOp Modulo          = mkMod
z3BinOp Minus           = app2 mkSub
z3BinOp LessThan        = mkLt
z3BinOp AtMost          = mkLe
z3BinOp GreaterThan     = mkGt
z3BinOp AtLeast         = mkGe
z3BinOp Xor             = mkXor
z3BinOp Implies         = mkImplies
z3BinOp EqualTo         = mkEq

z3NaryOp :: MonadZ3 m => NaryOperator -> [AST] -> m AST
z3NaryOp Times           = mkMul
z3NaryOp Plus            = mkAdd
z3NaryOp And             = mkAnd
z3NaryOp Or              = mkOr
