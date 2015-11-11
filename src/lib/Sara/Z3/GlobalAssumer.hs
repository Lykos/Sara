{-# LANGUAGE FlexibleContexts #-}

-- | Module that creates the global assumptions for the program, i.e. the definitions of functions, preconditions and postconditions.
module Sara.Z3.GlobalAssumer ( globalAssumptions ) where

import Control.Monad.Writer
import Control.Monad.State.Strict
import Sara.AstUtils
import Sara.Meta
import qualified Sara.Syntax as S
import qualified Sara.Z3.SymbolicState as S
import Sara.Z3.Utils
import Sara.Utils
import Sara.Z3.PureExpression
import Sara.Z3.CondAst
import Z3.Monad

-- TODO: Improve this.
globalAssumptions :: (MonadWriter [AST] m, MonadZ3 m) => PureCheckerProgram -> m ()
globalAssumptions prog = mapMDeclarations_ addDecl prog >> mapMSignatures_ addSig prog
  where addDecl :: (MonadWriter [AST] m, MonadZ3 m) => PureCheckerDeclaration -> m ()
        addDecl (S.Function S.Signature{ S.isPure = True, S.args = args, S.sigMeta = (m, _) } body _) = do
          body' <- z3FunctionBody args body
          args' <- z3Args args
          (_, _, func) <- z3FuncDecls m
          fApp <- mkApp func args'
          tell =<< (:[]) <$> mkEq fApp body'
        addDecl _                                                                                                          = return ()
        addSig ::  (MonadWriter [AST] m, MonadZ3 m) => PureCheckerSignature -> m ()
        addSig S.Signature{ S.args = args, S.preconditions = pres, S.postconditions = posts, S.sigMeta = (m, _) } = do
          pre' <- conditions args pres
          post' <- conditions args posts
          args' <- z3Args args
          (funcPre, funcPost, _) <- z3FuncDecls m
          preApp <- mkApp funcPre args'
          postApp <- mkApp funcPost args'
          tell =<< (:[]) <$> mkEq preApp pre'
          tell =<< (:[]) <$> mkEq postApp post'          
        conditions _ []       = mkTrue
        conditions args conds = mkAnd =<< mapM (z3FunctionBody args) conds
        z3Args args = mapM z3Arg args
        z3Arg (S.TypedVariable _ _ (m, _)) = z3Var m
        z3FunctionBody args body = evalStateT (ast <$> z3Expression body << setArgs args)
                                   $ S.empty (error "Start position of a global assumption should never be used.")
                                   (error "Entry type of a global assumption should never be used.")

setArgs :: (MonadState S.SymbolicState m, MonadZ3 m) => [PureCheckerTypedVariable] -> m ()
setArgs args = mapM_ setArg args
  where setArg (S.TypedVariable _ _ (m, _)) = S.setVar m =<< z3Var m


