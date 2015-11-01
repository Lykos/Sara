{-# LANGUAGE FlexibleContexts #-}

module Sara.PureChecker ( checkPureness ) where

import Sara.Syntax as S
import Sara.AstUtils
import Sara.Meta
import Sara.Operators
import Sara.Errors
import Data.Maybe
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

createPureMap :: SymbolizerProgram -> PureMap
createPureMap = foldMapSignatures sigPure
  where sigPure sig = M.singleton (fst $ sigMeta sig) (isPure sig)

checkSignatures :: SymbolizerProgram -> R.ReaderT PureMap (ExceptT Error Identity) ()
checkSignatures = mapMSignatures_ checkOneSignature
  where checkOneSignature Signature{ preconditions = precs, postconditions = posts } =
          checkConditions "precondition" precs >> checkConditions "postcondition" posts
        checkConditions name conds = mapM_ (checkPureExpression name) conds

checkPureBodies :: SymbolizerProgram -> R.ReaderT PureMap (ExceptT Error Identity) ()
checkPureBodies = mapMDeclarations_ checkPureBody
  where checkPureBody Extern{}                                                         = return ()
        checkPureBody Function{ signature = Signature{ isPure = False } }              = return ()
        checkPureBody Function{ body = body, signature = Signature{ sigName = name } } = checkPureExpression name body

checkPureExpression :: Name -> SymbolizerExpression -> R.ReaderT PureMap (ExceptT Error Identity)  ()
checkPureExpression name = mapMExpression_ checkPureSingleExpression
  where checkPureSingleExpression exp = do
          cond <- checkPure exp
          unless cond (lift $ impureExpression name $ expressionPos exp)
        
checkPure :: MonadReader PureMap r => SymbolizerExpression -> r Bool
checkPure S.Unit{}                            = return True
checkPure S.Boolean{}                         = return True
checkPure S.Integer{}                         = return True
checkPure S.Double{}                          = return True
checkPure UnaryOperation{}                    = return True
checkPure BinaryOperation{ binOp = Assign }   = return False
checkPure BinaryOperation{}                   = return True
checkPure Variable{}                          = return True
checkPure Call{ expCallMeta = sym }           = asks $ fromJust . (M.lookup sym)
checkPure Conditional{}                       = return True
checkPure Block{}                             = return True
checkPure _                                   = return False
