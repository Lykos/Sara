module TypeChecker where

import Types

data TypeError = TypeError String

addTypes :: [Signature] -> [TypedVariable] -> Expression -> Either TypeError Expression
addTypes vars sigs exp = case exp of
  b@(Boolean _) -> Right b
  n@(Integer _) -> Right n
  d@(Double _) -> Right d
  UnaryExpression op subExp _ =
    let typedSubExp = typedSubExp subExp
        subExpType = getType typedSubExp
    in addType (UnaryExpression op typedSubExp) (unOpType op subExpType)
  BinaryExpression op left right _ ->
    let typedLeft = typedSubExp left
        typedRight = typedSubExp right
        leftType = getType typedLeft
        rightType = getType typedRight
    in addType (BinaryExpression op typedLeft typedRight) (binOpType op leftType rightType)
  Variable name _ -> addType (Variable name) (varType name vars)
  Call name args _ ->
    let typedArgs = map typedSubExp args
        argTypes = map getType typedArgs
    in addType (Call name typedArgs) (funcType name argTypes sigs)
  Conditional cond ifExp elseExp ->
    let typedCond = typedSubExp cond
        typedIfExp = typedSubExp ifExp
        typedElseExp = typedSubExp elseExp
        condType = getType typedCond
        ifType = getType typedIfExp
        elseType = getType typedElseExp
    in case (condType, ifType, elseType) of
      (Boolean, ifType, elseType) | ifType == elseType    -> Conditional typedCond typedIfExp typedElseExp ifType
                                  | otherwise             -> TypeError "If and Else branch of conditionals have to have the same types, found " ++ show ifType ++ " and " ++ show elseType ++ "."
      (condType, _, _)                                    -> Conditional "Conditions of conditionals have to have type " ++ show (Boolean::Type) ++ ", found " ++ show condType
    where typedSubExp = addTypes sigs vars

addType :: (Type -> Expression) -> Either TypeError Type -> Either TypeError Expression
addType _ e@TypeError -> Left e
addType exp t@Type -> exp t

getType :: [Signature] -> [TypedVariable] -> Expression -> Type
getType Boolean _ = Boolean
getType Integer _ = Integer
getType Double _ = Double
getType UnaryOperation _ _ t = t
getType BinaryOperation _ _ _ t = t
getType Variable _ t = t
getType Call _ _ t = t
getType Conditional _ _ _ t = t

