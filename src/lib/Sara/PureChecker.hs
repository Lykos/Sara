{-# LANGUAGE FlexibleContexts #-}

module Sara.PureChecker ( checkPureness ) where

import Sara.Syntax as S
import Sara.AstUtils
import Sara.Meta
import Sara.Operators
import Sara.Errors
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as M

checkPureness :: SymbolizerProgram -> ErrorOr PureCheckerProgram
checkPureness prog = do
  prog' <- addPureAnnotations prog
  checkSignatures prog'
  checkPureBodies prog'
  checkPureAssertions prog'
  return prog'

addPureAnnotations :: SymbolizerProgram -> PureCheckerProgram
addPureAnnotations = mapExpressions addAnnotation . mapExpressionMetas changeMeta
  where changeMeta = mapExpressionMeta (\TypMeta t -> ExpressionMeta t (error "Pure annotation is not defined yet."))
        addAnnotation exp = exp{ expMeta = expMeta' }
          where expMeta' = ExpressionMeta (expressionTyp exp) (isPure exp)

checkSignatures :: PureCheckerProgram -> ErrorOr ()
checkSignatures = mapMSignatures_ checkOneSignature
  where checkOneSignature Signature{ isPure = isPure, sigName = name, preconditions = precs, postconditions = posts } =
          checkConditions (PurePrecondition func) precs >> checkConditions (PurePostcondition func) posts
          where func = functionOrMethod isPure name
        checkConditions context conds = mapM_ (checkPureExpression context) conds

checkPureBodies :: PureCheckerProgram -> ErrorOr ()
checkPureBodies = mapMDeclarations_ checkPureBody
  where checkPureBody S.Function{ body = body, signature = Signature{ isPure = True, sigName = name } } =| 
          checkPureExpression (PureFunction name) body
        checkPureBody _                                                                                 =
          return ()

checkPureAssertions :: PureCheckerProgram -> ErrorOr ()
checkPureAssertions = mapMExpressions_ checkOneExpression
  where checkOneExpression S.Assertion{ assertionKind = kind, inner = exp } = checkPureExpression (PureAssertion kind) exp
        checkOneExpression S.While{ invariants = invs }                     = mapM_ (checkPureExpression PureInvariant) invs
        checkOneExpression _                                                = return ()

checkPureExpression :: PureContext -> PureCheckerExpression -> ErrorOr ()
checkPureExpression context exp | not isPureExp $ expMeta exp = impureExpression context $ expressionPos exp
checkPureExpression _                                         = return ()

-- | Checks if the given expression is pure.
isPure :: PureCheckerExpression -> Bool
isPure exp = all (isPureExp . expMeta) (children exp) && isPure' exp
  where isPure' S.Unit{}                          = True
        isPure' S.Boolean{}                       = True
        isPure' S.Integer{}                       = True
        isPure' S.Double{}                        = True
        isPure' UnaryOperation{}                  = True
        isPure' BinaryOperation{ binOp = Assign } = False
        isPure' BinaryOperation{}                 = True
        isPure' Variable{}                        = True
        isPure' Call{ expCallMeta = sym }         = isPureSym sym
        isPure' Conditional{}                     = True
        isPure' Block{}                           = True
        isPure' _                                 = False
