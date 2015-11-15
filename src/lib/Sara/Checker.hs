{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Sara.Checker ( checkWithoutMain
                    , checkWithMain ) where

import Text.Parsec.Pos
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Monoid
import qualified Data.Map as M
import Sara.AstUtils
import Sara.Meta
import qualified Sara.Builtins as B
import Sara.Operators
import Sara.Syntax
import Sara.Errors
import qualified Sara.Types as T

checkWithoutMain :: Program a b c NodeMeta -> ErrorOr ()
checkWithoutMain = checkProgram

checkWithMain :: Program a b c NodeMeta -> ErrorOr ()
checkWithMain p = checkMain p >> checkProgram p

checkMain :: Program a b c NodeMeta -> ErrorOr ()
checkMain program = unless (hasMain program) noMain

hasMain :: Program a b c d -> Bool
hasMain = getAny . foldMapSignatures (\s -> Any $ sigName s == "main")

checkProgram :: Program a b c NodeMeta -> ErrorOr ()
checkProgram p = checkSignatures p >> checkAssignments p

checkSignatures :: Program a b c NodeMeta -> ErrorOr ()
checkSignatures = mapMSignatures_ checkSignature
  where checkSignature Signature{ sigName = "main", args = [], retType = T.Integer } = return ()
        checkSignature s@Signature{ sigName = "main", args = [], retType = t }       = invalidMainRetType t (signaturePos s)
        checkSignature s@Signature{ sigName = "main", args = a }                     = invalidMainArgs (map varType a) (signaturePos s)
        checkSignature Signature{..}                                                 = checkArgs args $ functionOrMethod isPure sigName

checkArgs :: [TypedVariable b NodeMeta] -> FunctionOrMethod -> ErrorOr ()
checkArgs args functionOrMethod = evalStateT (mapM_ checkArg args) M.empty
  where checkArg :: TypedVariable b NodeMeta -> StateT (M.Map Name SourcePos) (ExceptT Error Identity) ()
        checkArg var = do
          let name = varName var
          let pos = typedVarPos var
          case B.builtinVar name of
            Just B.Result -> lift $ resultArg pos
            _             -> return ()
          previousPos <- gets $ M.lookup name
          case previousPos of
            Just pos' -> lift $ redeclaredArgument name functionOrMethod pos' pos
            Nothing   -> return ()
          modify $ M.insert name pos
          return ()

checkAssignments :: Program a b c NodeMeta -> ErrorOr ()
checkAssignments = mapMExpressions_ checkAssignment
  where checkAssignment BinaryOperation{ left = left, binOp = Assign } = checkAssignable left
        checkAssignment _                                              = return ()
        checkAssignable Variable{} = return ()
        checkAssignable a          = notAssignable $ expressionPos a
