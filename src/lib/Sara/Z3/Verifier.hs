{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Sara.Z3.Verifier ( verify ) where

import Sara.Errors as E
import Sara.AstUtils
import Sara.Z3.AstWrapper
import Sara.Z3.SymbolicExecutor
import Control.Monad.Except
import Control.Monad.Writer
import qualified Sara.Z3.ProofPart as P
import Sara.Meta
import Sara.Z3.GlobalAssumer
import Z3.Monad

verify :: MonadZ3 m => PureCheckerProgram -> ExceptT Error m ()
verify prog = addGlobalAssumptions prog >> verify' prog

addGlobalAssumptions :: MonadZ3 m => PureCheckerProgram -> m ()
addGlobalAssumptions prog = do
  assumptions <- execWriterT (globalAssumptions prog)
  mapM_ assert assumptions

generateProofParts :: MonadZ3 m => PureCheckerProgram -> m [P.ProofPart]
generateProofParts prog = do
  execWriterT (mapMDeclarations_ symbolicExecuteDecl prog)

verify' :: MonadZ3 m => PureCheckerProgram -> ExceptT Error m ()
verify' prog = do
  proofParts <- generateProofParts prog
  mapM_ verifyProofPart proofParts
  where verifyProofPart :: MonadZ3 m => P.ProofPart -> ExceptT Error m ()
        verifyProofPart p@P.ProofPart{..} = local $ do
          assert =<< runAst assumption
          assert =<< mkNot =<< runAst proofObligation
          res <- lift $ withModel $ \model -> do
            failure <- P.findFailure model p
            m <- modelToString model
            liftIO $ putStrLn $ "model:\n" ++ m
            so <- solverToString
            liftIO $ putStrLn $ "solver:\n" ++ so
            return failure
          case res of
            (Sat, Just (Just e)) -> throwError e
            (Sat, Just Nothing)  -> error "There was a failure, but we were not able to find it in the model."
            (Sat, Nothing)       -> error "Satisfiable, but no model attached."
            (Unsat, _)           -> return ()
            (Undef, _)           -> unsolvableError startType startPos
