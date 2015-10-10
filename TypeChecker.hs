module TypeChecker where

import Types

data TypeError = String

checkType :: Expression -> Either Type TypeError
checkType = case Boolean 
