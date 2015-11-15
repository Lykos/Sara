{-# LANGUAGE RecordWildCards #-}
 
-- | Representation of one thing that has to be proven. Responsible for bundling the data and constructing an error in case of failure.

module Sara.Z3.ProofPart ( ProofPart(..)
                         , substituteFuncs
                         , findFailure ) where

import qualified Sara.Z3.AstWrapper as W
import qualified Data.Map as M
import qualified Sara.Errors as E
import qualified Sara.Z3.Ast as A
import Sara.Z3.CodeGenerator ( codegen )
import Sara.Meta
import Z3.Monad
import Text.Parsec.Pos

data ProofPart
  = ProofPart { startPos :: SourcePos
              , startType :: E.SymbolicExecutionStart
              , variableInitialStates :: M.Map VariableMeta A.Ast
              , assumption :: W.Assumption
              , proofObligation :: W.ProofObligation }
  deriving (Eq, Ord, Show)

substituteFuncs :: M.Map A.AppMeta ([VariableMeta], A.Ast) -> ProofPart -> ProofPart
substituteFuncs funcs p@ProofPart{..} = p{ assumption = W.substituteFuncs funcs assumption
                                         , proofObligation = W.substituteFuncs funcs proofObligation }

findFailure :: MonadZ3 m => Model -> ProofPart -> m (Maybe E.Error)
findFailure model ProofPart{..} = do
  model' <- E.VerifierFailureModel <$> evaluateVars model variableInitialStates
  failureInfo <- W.findFailure model $ W.runProofObligation proofObligation
  case failureInfo of
        Nothing              -> return Nothing
        Just (failure, pos)  -> return $ Just $ E.PositionedError (E.VerifierError startType startPos model' failure) pos
  where evaluateVars model vars = mapM (evaluateVar model) (M.toList vars)
        evaluateVar model (m, var) = do
          let name = varSymName m
          var' <- codegen var
          val <- eval model var'
          val' <- case val of
            Nothing -> do
              s <- modelToString model
              error $ "Unable to evaluate variable " ++ name ++ " in the following model:\n" ++ s
            Just v  -> astToString v
          return (name, val')
