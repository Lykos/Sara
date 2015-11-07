{-# LANGUAGE FlexibleContexts #-}

module Sara.Z3.Verifier ( verify ) where

import Sara.Errors as E
import Sara.AstUtils
import Sara.Z3.AstWrapper
import Sara.Z3.Utils
import Sara.Z3.PureExpression
import Control.Monad.Except
import qualified Sara.Syntax as S
import Sara.Meta
import Sara.Z3.CondAst
import Z3.Monad

instance MonadZ3 m => MonadZ3 (ExceptT a m) where
  getSolver = lift getSolver
  getContext = lift getContext

verify :: MonadZ3 z3 => SymbolizerProgram -> ExceptT Error z3 ()
verify prog = defineEverything prog >> verifyFunctions prog

defineEverything :: MonadZ3 z3 => SymbolizerProgram -> z3 ()
defineEverything prog = mapMDeclarations_ addDecl prog >> mapMSignatures_ addSig prog
  where addDecl ::  MonadZ3 z3 => SymbolizerDeclaration -> z3 ()
        addDecl (S.Function S.Signature{ S.isPure = True, S.args = args, S.retType = retType, S.sigMeta = (m, _) } body _) = do
          body' <- z3ExpressionAst body
          args' <- z3Args args
          (_, _, func) <- z3FuncDecls m (map S.varType args) retType
          fApp <- mkApp func args'
          assert =<< mkEq fApp body'
        addDecl _                                                                                                          = return ()
        addSig ::  MonadZ3 z3 => SymbolizerSignature -> z3 ()
        addSig S.Signature{ S.args = args, S.retType = retType, S.preconditions = pres, S.postconditions = posts, S.sigMeta = (m, _) } = do
          pre' <- conditions pres
          post' <- conditions posts
          args' <- z3Args args
          (funcPre, funcPost, _) <- z3FuncDecls m (map S.varType args) retType
          preApp <- mkApp funcPre args'
          postApp <- mkApp funcPost args'
          assert =<< mkEq preApp pre'
          assert =<< mkEq postApp post'          
        conditions []    = mkTrue
        conditions conds = mkAnd =<< mapM z3ExpressionAst conds
        z3ExpressionAst e = ast <$> z3Expression e
        z3Args args = mapM z3Arg args
        z3Arg (S.TypedVariable _ t (m, _)) = z3Var m t

verifyFunctions :: MonadZ3 z3 => SymbolizerProgram -> ExceptT Error z3 ()
verifyFunctions = mapMDeclarations_ verifyDecl
  where verifyDecl :: MonadZ3 z3 => SymbolizerDeclaration -> ExceptT Error z3 ()
        verifyDecl (S.Function sig@S.Signature{ S.args = args, S.isPure = True, S.preconditions = pres, S.postconditions = posts } body _) = local $ do
          (prePre, postPre, pre) <- preconditions pres
          (prePost, postPost, post) <- postconditions posts
          (preBody, postBody, _) <- runCondAst =<< z3Expression body
          -- We can assume that the precondition holds.
          assert pre
          -- We can also assume that the postconditions all hold
          assert postPre
          assert postPost
          assert postBody
          let evalModel = evalArgs args
          -- Now we assert that all the preconditions of our components and our postcondition holds.
          local $ assertHolds PrePrecondition sig evalModel prePre
          local $ assertHolds PostPrecondition sig evalModel prePost
          local $ assertHolds BodyPrecondition sig evalModel preBody
          local $ assertHolds Postcondition sig evalModel post
        verifyDecl _                                                                                                    = return ()
        evalArgs :: MonadZ3 z3 => [SymbolizerTypedVariable] -> Model -> z3 VerifierFailureModel
        evalArgs args model = VerifierFailureModel <$> mapM (evalArg model) args
        evalArg :: MonadZ3 z3 => Model -> SymbolizerTypedVariable -> z3 (S.Name, String)
        evalArg model S.TypedVariable{ S.varType = typ, S.varName = name, S.varMeta = (m, _) } = do
          var <- z3Var m typ
          val <- eval model var
          val' <- case val of
            Nothing -> do
              s <- modelToString model
              error $ "Unable to evaluate variable " ++ name ++ " in the following model:\n" ++ s
            Just v  -> astToString v
          return (name, val')
        preconditions :: MonadZ3 z3 => [SymbolizerExpression] -> z3 (FailureTrackableAST, AST, AST)
        preconditions []    = runCondAst =<< trivial =<< mkTrue
        preconditions [exp] = runCondAst =<< z3Expression exp
        preconditions conds = runCondAst =<< (combine mkAnd =<< (mapM z3Expression conds))
        postconditions :: MonadZ3 z3 => [SymbolizerExpression] -> z3 (FailureTrackableAST, AST, FailureTrackableAST)
        postconditions conds = do
          conds' <- mapM z3Expression conds
          (prePosts, postPosts, posts) <- unzip3 <$> mapM runCondAst conds'
          let failures = map postconditionFailure conds
          let prePost = conjunct prePosts
          postPost <- conjunctAsts postPosts
          let post = conjunct $ zipWith singleton posts failures
          return (prePost, postPost, post)
        postconditionFailure exp = (PostconditionViolation, expressionPos exp)
        assertHolds :: MonadZ3 z3 => ContextType -> SymbolizerSignature -> (Model -> z3 VerifierFailureModel) -> FailureTrackableAST -> ExceptT Error z3 ()
        assertHolds contextType sig evalModel assertion = do
          -- Assume that the assertion is NOT true.
          assert =<< mkNot =<< runAst assertion
          res <- lift $ withModel $ \model -> do
            model' <- evalModel model
            failure <- findFailure model assertion
            s <- astToString =<< runAst assertion
            liftIO $ putStrLn $ "ast:\n" ++ s
            m <- modelToString model
            liftIO $ putStrLn $ "model:\n" ++ m
            so <- solverToString
            liftIO $ putStrLn $ "solver:\n" ++ so
            return (model', failure)
          let func = functionOrMethod (S.isPure sig) (S.sigName sig)
          case res of
            (Sat, Just (model, Just (typ, pos))) -> verifierError contextType func model typ pos
            (Sat, Just (_, Nothing))             -> error "There was a failure, but we were not able to find it in the model."
            (Sat, Nothing)                       -> error "Satisfiable, but no model attached."
            (Unsat, _)                           -> return ()
            (Undef, _)                           -> unsolvableError contextType func $ signaturePos sig
