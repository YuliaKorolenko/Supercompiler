{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
module Driving where

import qualified Data.Map as M
import Debug.Trace
import Ast
import Decompose
import Decomposition

driving :: Program -> Expr -> ProcessTree
driving prgrm expr = case decompose expr of
    Left e@(Con cnstr branches) -> Node e (map (driving prgrm) branches)
    Left (Var _) -> Node expr []
    Left e@(Lam _ body) -> Node e [driving prgrm body]
    Right (ctx, redex) ->  Node expr (applyDrivingRuleForRight prgrm ctx redex)


applyDrivingRuleForRight :: Program -> Context -> Red -> [ProcessTree]
applyDrivingRuleForRight prgrm ctx (RFun funName) = 
    case lookupDef prgrm funName of
        Just body -> [driving prgrm (recompose ctx body)]
        Nothing   -> error $ "Function not defined: " ++ funName
applyDrivingRuleForRight prgrm ctx (RApp (Lam (Variable v) body, e2)) = let env = M.singleton v e2
                                                                        in [driving prgrm (substitute env body)]
applyDrivingRuleForRight prgrm ctx (RCase (Con c args, branches)) = 
    case findBranch c branches of  
        Just (Branch (PCon _ vars) eBr) ->
                    let env = M.fromList (zip (map (\(Variable v) -> v) vars) args)
                    in [Node (substitute env eBr) []]
        Nothing -> error $ "No branch for constructor: " ++ show c
applyDrivingRuleForRight prgrm ctx (RCase (e, branches)) =
    map (\(Branch _ brExpr) -> driving prgrm brExpr) branches

lookupDef :: Program -> String -> Maybe Expr
lookupDef (Program smth ((Definition name expr) : defs)) f_name =
    if name == f_name
    then Just expr
    else lookupDef (Program smth defs) f_name
lookupDef (Program _ []) _ = Nothing


substitute :: M.Map String Expr -> Expr -> Expr
substitute env (Case expr branches) = Case (substitute env expr) (map (substituteBranch env) branches)
substitute env v@(Var (Variable varName)) =  M.findWithDefault v varName env
substitute env (Con c args) = Con c (map (substitute env) args)
substitute env (App e1 e2) = App (substitute env e1) (substitute env e2)
substitute _ f@(Fun _) = f
substitute env (Lam (Variable v) e) =
  let env' = M.delete v env  -- Avoid capturing bound variable
  in Lam (Variable v) (substitute env' e)

substituteBranch :: M.Map String Expr -> Branch -> Branch
substituteBranch env (Branch pattern expr) =
    let patVars = case pattern of
                  PCon _ vs -> map (\(Variable v) -> v) vs
        env' = foldr M.delete env patVars
    in Branch pattern (substitute env' expr)

findBranch :: Cnstructor -> [Branch] -> Maybe Branch
findBranch _ [] = Nothing
findBranch constr (b@(Branch (PCon c _) _) : bs)
    | constr == c = Just b
    | otherwise   = findBranch constr bs