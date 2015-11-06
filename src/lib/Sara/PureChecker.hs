{-# LANGUAGE FlexibleContexts #-}

module Sara.PureChecker ( checkPureness ) where

import Sara.Syntax as S
import Sara.AstUtils
import Sara.Meta
import Sara.Operators
import Sara.Errors
import Control.Monad
import Control.Monad.Reader.Class
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as M

type PureMap = M.Map FunctionMeta Bool

checkPureness :: SymbolizerProgram -> ErrorOr ()
checkPureness prog = R.runReaderT (checkPureness' prog) (createPureMap prog)

checkPureness' :: SymbolizerProgram -> R.ReaderT PureMap (ExceptT Error Identity)  ()
checkPureness' prog = do
  checkSignatures prog
  checkPureBodies prog
  checkPureAssertions prog

createPureMap :: SymbolizerProgram -> PureMap
createPureMap = foldMapSignatures sigPure
  where sigPure sig = M.singleton (fst $ sigMeta sig) (isPure sig)

checkSignatures :: SymbolizerProgram -> R.ReaderT PureMap (ExceptT Error Identity) ()
checkSignatures = mapMSignatures_ checkOneSignature
  where checkOneSignature Signature{ isPure = isPure, sigName = name, preconditions = precs, postconditions = posts } =
          checkConditions (PurePrecondition func) precs >> checkConditions (PurePostcondition func) posts
          where func = functionOrMethod isPure name
        checkConditions name conds = mapM_ (checkPureExpression name) conds

checkPureBodies :: SymbolizerProgram -> R.ReaderT PureMap (ExceptT Error Identity) ()
checkPureBodies = mapMDeclarations_ checkPureBody
  where checkPureBody Extern{}                                                           = return ()
        checkPureBody S.Function{ signature = Signature{ isPure = False } }              = return ()
        checkPureBody S.Function{ body = body, signature = Signature{ sigName = name } } = checkPureExpression (PureFunction name) body

checkPureAssertions :: SymbolizerProgram -> R.ReaderT PureMap (ExceptT Error Identity) ()
checkPureAssertions = mapMExpressions_ checkOneExpression
  where checkOneExpression S.Assertion{ assertionKind = kind, inner = exp } = checkPureExpression (PureAssertion kind) exp
        checkOneExpression _                                                = return ()

checkPureExpression :: PureContext -> SymbolizerExpression -> R.ReaderT PureMap (ExceptT Error Identity)  ()
checkPureExpression context = mapMExpression_ checkPureSingleExpression
  where checkPureSingleExpression exp = do
          cond <- checkPure exp
          unless cond (lift $ impureExpression context $ expressionPos exp)
        
checkPure :: MonadReader PureMap r => SymbolizerExpression -> r Bool
checkPure S.Unit{}                            = return True
checkPure S.Boolean{}                         = return True
checkPure S.Integer{}                         = return True
checkPure S.Double{}                          = return True
checkPure UnaryOperation{}                    = return True
checkPure BinaryOperation{ binOp = Assign }   = return False
checkPure BinaryOperation{}                   = return True
checkPure Variable{}                          = return True
checkPure Call{ expCallMeta = sym }           = asks $ \funcs -> case sym `M.lookup` funcs of
  Just b  -> b
  Nothing -> error $ "Call symbol " ++ show sym ++ " not in pure table."
checkPure Conditional{}                       = return True
checkPure Block{}                             = return True
checkPure _                                   = return False
