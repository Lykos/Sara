{-# LANGUAGE RecordWildCards #-}

module Sara.Transform.VarDefRemover ( removeVarDefs ) where

import qualified Sara.Ast.AstUtils as A
import qualified Sara.Ast.Syntax as S
import qualified Sara.Ast.Operators as O
import qualified Sara.Ast.Meta as M

removeVarDefs :: M.PureCheckerProgram -> M.PureCheckerProgram
removeVarDefs = A.mapExpressions removeVarDef
  where removeVarDef S.VarDef{..} = S.BinaryOperation O.Assign (varFromTypedVar typedVar) inner expMeta expNodeMeta
        removeVarDef e            = e
        varFromTypedVar S.TypedVariable{..} = S.Variable varName varMeta (M.ExpressionMeta varType True) varNodeMeta
