module Sara.Checker ( checkWithoutMain
                    , checkWithMain ) where

import Control.Monad
import Data.Monoid
import Sara.AstUtils
import Sara.Meta
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
        checkSignature _                                                             = return ()

checkAssignments :: Program a b c NodeMeta -> ErrorOr ()
checkAssignments = mapMExpressions_ checkAssignment
  where checkAssignment BinaryOperation{ left = left, binOp = Assign } = checkAssignable left
        checkAssignment _                                              = return ()
        checkAssignable Variable{} = return ()
        checkAssignable a          = notAssignable $ expressionPos a
