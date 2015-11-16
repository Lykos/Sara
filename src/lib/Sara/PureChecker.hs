{-# LANGUAGE FlexibleContexts #-}

module Sara.PureChecker ( checkPureness ) where

import Sara.Syntax as S
import Sara.AstUtils
import Sara.Meta
import Sara.Operators
import Sara.Errors
import Text.Parsec.Pos
import Data.Maybe

checkPureness :: SymbolizerProgram -> ErrorOr PureCheckerProgram
checkPureness prog = do
  let prog' = addPureAnnotations prog
  checkSignatures prog'
  checkPureBodies prog'
  checkPureAssertions prog'
  return prog'

addPureAnnotations :: SymbolizerProgram -> PureCheckerProgram
addPureAnnotations = mapExpressions addAnnotation . mapExpressionMetas changeMeta
  where changeMeta :: TypMeta -> ExpressionMeta
        changeMeta = (\(TypMeta t) -> ExpressionMeta t (error "Pure annotation is not defined yet."))
        addAnnotation :: PureCheckerExpression -> PureCheckerExpression
        addAnnotation exp = exp{ expMeta = expMeta' }
          where expMeta' = ExpressionMeta (expressionTyp exp) (isPureLol exp)

checkSignatures :: PureCheckerProgram -> ErrorOr ()
checkSignatures = mapMSignatures_ checkOneSignature
  where checkOneSignature Signature{ S.isPure = isPure, sigName = name, preconditions = precs, postconditions = posts } =
          checkConditions (PurePrecondition func) precs >> checkConditions (PurePostcondition func) posts
          where func = functionOrMethod isPure name
        checkConditions context conds = mapM_ (checkPureExpression context) conds

checkPureBodies :: PureCheckerProgram -> ErrorOr ()
checkPureBodies = mapMDeclarations_ checkPureBody
  where checkPureBody S.Function{ body = body, signature = Signature{ S.isPure = True, sigName = name } } = 
          checkPureExpression (PureFunction name) body
        checkPureBody _                                                                                   =
          return ()

checkPureAssertions :: PureCheckerProgram -> ErrorOr ()
checkPureAssertions = mapMExpressions_ checkOneExpression
  where checkOneExpression S.Assertion{ assertionKind = kind, inner = exp } = checkPureExpression (PureAssertion kind) exp
        checkOneExpression S.While{ invariants = invs }                     = mapM_ (checkPureExpression PureInvariant) invs
        checkOneExpression _                                                = return ()

checkPureExpression :: PureContext -> PureCheckerExpression -> ErrorOr ()
checkPureExpression context exp = case findImpurePos exp of
  Just pos -> impureExpression context pos
  Nothing  -> return ()

findImpurePos :: PureCheckerExpression -> Maybe SourcePos
findImpurePos exp | expressionPure exp          = Nothing
                  | not $ preservesPureness exp = Just (expressionPos exp)
                  | otherwise                   = listToMaybe $ catMaybes $ map findImpurePos $ children exp

--  | Checks if the given expression preserves pureness and has only pure children.
isPureLol :: PureCheckerExpression -> Bool
isPureLol exp = all expressionPure (children exp) && preservesPureness exp

preservesPureness :: PureCheckerExpression -> Bool
preservesPureness S.Unit{}                          = True
preservesPureness S.Boolean{}                       = True
preservesPureness S.Integer{}                       = True
preservesPureness S.Double{}                        = True
preservesPureness UnaryOperation{}                  = True
preservesPureness BinaryOperation{ binOp = Assign } = False
preservesPureness BinaryOperation{}                 = True
preservesPureness Variable{}                        = True
preservesPureness Call{ expCallMeta = sym }         = funcSymPure sym
preservesPureness Conditional{}                     = True
preservesPureness Block{}                           = True
preservesPureness _                                 = False
