module Sara.PureChecker ( checkPureness ) where

import Sara.Syntax as S
import Sara.AstUtils
import Sara.Meta
import Sara.Operators
import Sara.Errors
import Sara.Symbols
import qualified Data.Map.Strict as M
import Data.Maybe

checkPureness :: FunctionMap -> TypeCheckerProgram -> ErrorOr ()
checkPureness funcs prog = checkSignatures funcs prog >> checkPureBodies funcs prog

checkSignatures :: FunctionMap -> TypeCheckerProgram -> ErrorOr ()
checkSignatures funcs = mapMSignatures_ checkOneSignature
  where checkOneSignature Signature{ preconditions = precs, postconditions = posts } =
          checkConditions "precondition" precs >> checkConditions "postcondition" posts
        checkConditions name conds = mapM_ (checkPureExpression funcs name) conds

checkPureBodies :: FunctionMap -> TypeCheckerProgram -> ErrorOr ()
checkPureBodies funcs = mapMDeclarations_ checkPureBody
  where checkPureBody Extern{}                                                         = return ()
        checkPureBody Function{ signature = Signature{ isPure = False } }              = return ()
        checkPureBody Function{ body = body, signature = Signature{ sigName = name } } = checkPureExpression funcs name body

checkPureExpression :: FunctionMap -> Name -> TypeCheckerExpression -> ErrorOr ()
checkPureExpression funcs name = mapMExpression_ checkPureSingleExpression
  where checkPureSingleExpression exp = if isPure exp then return () else impureExpression name (expressionPos exp)
        isPure :: TypeCheckerExpression -> Bool
        isPure S.Unit{}                            = True
        isPure S.Boolean{}                         = True
        isPure S.Integer{}                         = True
        isPure S.Double{}                          = True
        isPure UnaryOperation{}                    = True
        isPure BinaryOperation{ binOp = Assign }   = False
        isPure BinaryOperation{}                   = True
        isPure Variable{}                          = True
        isPure Call{ expName = n, expArgs = args } = S.isPure sig
          where sig :: ParserSignature
                sig = fromJust $ FunctionKey n (map expressionTyp args) `M.lookup` funcs
        isPure Conditional{}                       = True
        isPure Block{}                             = True
        isPure _                                   = False
