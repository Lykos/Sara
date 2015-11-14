{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Sara.Z3.Declarations ( translateContracts, translateFuncDefs ) where

import Control.Monad.State.Strict
import Sara.Meta
import Sara.Utils
import Sara.AstUtils
import Data.Maybe
import qualified Sara.Z3.Ast as A
import qualified Sara.Z3.CondAst as C
import qualified Sara.Z3.SymbolicState as S
import qualified Sara.Syntax as S
import qualified Data.Graph as G
import qualified Data.Map as M
import Sara.Z3.PureExpression
import Text.Parsec.Pos

-- Translates all function definitions.
translateFunctions :: M.Map A.AppMeta ([VariableMeta], A.Ast) -> PureCheckerProgram -> [(A.AppMeta, ([VariableMeta], A.Ast))]
translateFunctions contracts = foldMapDeclarations translateFunc
  where translateFunc S.Function{..} | S.isPure signature = [ (A.AppMeta A.FuncApp (fst $ S.sigMeta signature),
                                                               (map (fst . S.varMeta) args, translate (nodePos declMeta) args body)) ]
          where args = S.args signature
        translateFunc _                                   = []
        translate pos args exp = A.substituteFuncs contracts $ C.ast $ withInitialState args pos (translateExpression exp)

-- Translates all contracts.
translateContracts :: PureCheckerProgram -> M.Map A.AppMeta ([VariableMeta], A.Ast)
translateContracts = M.fromList . foldMapSignatures translateSig
        -- TODO: Use AstWrapper types for precondition and postcondition
  where translateSig S.Signature{..} = [ translateConditions A.PreApp preconditions
                                       , translateConditions A.PostApp postconditions ]
          where pos = nodePos $ snd sigMeta
                translateConditions appKind conditions = (A.AppMeta appKind $ fst sigMeta,
                                                          (map (fst . S.varMeta) args, translateN args conditions))
                translateN args conds = A.NaryOp A.And $ map C.ast $ withInitialState args pos (mapM translateExpression conds)

withInitialState :: [PureCheckerTypedVariable] -> SourcePos -> State S.SymbolicState a -> a
withInitialState args pos f = evalState (setArgs >> f) initialState
  where initialState = (S.empty pos S.AfterMethodEntry)
        setArgs = mapM_ (setArg . fst . S.varMeta) args
        setArg v = S.setVar v (A.Var v)

-- | Creates function definitions for all functions in the map using a quantified formula for each function.
-- It only creates the definitions for the actual function definitions, not for the preconditions and postconditions.
translateFuncDefs :: M.Map A.AppMeta ([VariableMeta], A.Ast) -> PureCheckerProgram -> [A.Ast]
translateFuncDefs contracts prog = concatMap createDefs funcList
  where (graph, vertexToNode, keyToVertex) = constructCallGraph
        -- Constructs the call graph on the function metadata where a connection from a to b exists if the ast belonging to a contains b.
        constructCallGraph = G.graphFromEdges $ catMaybes $ map constructGraphNode funcList
        -- Construct a graph node for one function application.
        constructGraphNode (A.AppMeta A.FuncApp m, _) = Just (m, m, called m)
        constructGraphNode _                          = Nothing
        -- Returns the list of functions reachable by a given function.
        reachable :: FunctionMeta -> [FunctionMeta]
        reachable f = case M.lookup f reachableMap of
          Just l  -> l
          Nothing -> error $ "Function " ++ show f ++ " not found in the reachable map."
        -- Create a map from function metadata to the list of reachable function metadatas.
        reachableMap = M.fromList $ map reachableFuncs metaList
        reachableFuncs m = case keyToVertex m of
          Just v  -> (m, map extractMeta $ G.reachable graph v)
          Nothing -> error $ "Function " ++ show m ++ " not found in the call graph."
          where extractMeta = tripleSnd . vertexToNode
        -- Finds the dangerous called functions (i.e. the ones that can lead to a cycle).
        dangerousMetas :: FunctionMeta -> [FunctionMeta]
        dangerousMetas m = filter (elem m . reachable) (called m)
        -- Create one quantified definition of a function.
        createDefs (A.AppMeta A.FuncApp m, (args, ast)) = [ quantifyCall m args $ A.simplify $ substituteFakes (dangerousMetas m) ast
                                                          , quantifyCall m args $ A.App (A.AppMeta A.FakeApp m) $ map A.Var args ]
        createDefs _                                    = []
        funcList = translateFunctions contracts prog
        metaList :: [FunctionMeta]
        metaList = map extractMeta funcList
          where extractMeta (A.AppMeta A.FuncApp m, _) = m
                extractMeta (a, _)                     = error $ "Extracted meta for illegal function " ++ show a
        -- Returns the list of functions called by a given function.
        called f = case M.lookup f calledFuncsMap of
          Just l  -> l
          Nothing -> error $ "Function " ++ show f ++ " not found in the neighbor map."
        -- Returns a list of the function metas used in an ast.
        calledFuncsMap = M.fromList $ map extractCalledFuncs funcList
        extractCalledFuncs (A.AppMeta A.FuncApp m, (_, ast)) = (m, calledFuncs ast)
        extractCalledFuncs a                                 = error $ "Called functions for illegal function " ++ show a

calledFuncs :: A.Ast -> [FunctionMeta]
calledFuncs ast = calledFuncsDirect ast ++ concatMap calledFuncs (A.children ast)
  where calledFuncsDirect (A.App (A.AppMeta A.FuncApp m) _) = [m]
        calledFuncsDirect _                                 = []

-- | Creates a quantified formula that says that calling the function with the given arguments is equal to the given ast.
quantifyCall :: FunctionMeta -> [VariableMeta] -> A.Ast -> A.Ast
quantifyCall m args ast = A.Forall args (A.BinOp A.EqualTo mApp ast) [A.Pattern [mApp]]
  where mApp = A.App (A.AppMeta A.FuncApp m) $ map A.Var args

-- | Substitutes the given called functions by fake called functions.
substituteFakes :: [FunctionMeta] -> A.Ast -> A.Ast
substituteFakes _  b@A.BoolConst{}    = b
substituteFakes _  n@A.IntConst{}     = n
substituteFakes _  v@A.Var{}          = v
substituteFakes ms (A.App a bs)       = let args = map (substituteFakes ms) bs in case a of
  A.AppMeta A.FuncApp m | m `elem` ms -> A.App (A.AppMeta A.FakeApp m) args
  _                                   -> A.App a args
substituteFakes ms (A.UnOp op a)      = A.UnOp op $ substituteFakes ms a
substituteFakes ms (A.BinOp op a b)   = A.BinOp op (substituteFakes ms a) (substituteFakes ms b)
substituteFakes ms (A.NaryOp op bs)   = A.NaryOp op $ map (substituteFakes ms) bs
substituteFakes ms (A.Ite a b c)      = A.Ite (substituteFakes ms a) (substituteFakes ms b) (substituteFakes ms c)
substituteFakes ms (A.Forall as b cs) = A.Forall as (substituteFakes ms b) $ map (\(A.Pattern ps) -> A.Pattern $ map (substituteFakes ms) ps) cs
