{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Driving where

import qualified Data.Map as M
import Debug.Trace
import Ast

drive :: Program -> Expr -> Expr
drive _ (Var v) = Var v
drive prgrm (Con c exprs) = Con c (map (drive prgrm) exprs)
drive prgrm (Lam v expr) = Lam v (drive prgrm expr)
drive prgrm (Fun f) = case lookupDef prgrm f of
                        Just body -> body
                        Nothing   -> Fun f
drive prgrm (App e1 e2) =
    let e1' = drive prgrm e1
    in case e1' of
        Lam (Variable v) eBody ->
            -- Beta reduction
            let env = M.singleton v e2
            in substitute env eBody
        _ -> App e1' (drive prgrm e2)
drive prgrm (Case e branches) =
    let e' = drive prgrm e
    in case e' of
        Con c args ->
            case findBranch c branches of
                Just (Branch (PCon _ vars) eBr) ->
                    let env = M.fromList (zip (map (\(Variable v) -> v) vars) args)
                    in substitute env eBr
                Nothing -> error $ "No branch for constructor: " ++ show c
        _ -> Case e' (map (driveBranch prgrm) branches)
    where
        driveBranch program (Branch pat eBr) =
            Branch pat (drive program eBr)



lookupDef :: Program -> String -> Maybe Expr
lookupDef (Program smth ((Definition name expr) : defs)) f_name =
    if name == f_name
    then Just expr
    else lookupDef (Program smth defs) f_name


substitute :: M.Map String Expr -> Expr -> Expr
substitute env (Case expr branches) = Case (substitute env expr) (map (substituteBranch env) branches)
substitute env v@(Var (Variable varName)) =  M.findWithDefault v varName env
substitute env (Con c args) = Con c (map (substitute env) args)
substitute env (App e1 e2) = App (substitute env e1) (substitute env e2)
substitute _ f@(Fun _) = f
substitute env expr1 = trace ("Env: " ++ show env ++ "Expr1: " ++ show expr1)
                       undefined

substituteBranch :: M.Map String Expr -> Branch -> Branch
substituteBranch env (Branch pattern expr) = Branch pattern (substitute env expr) -- maybe i shoud do something with env


findBranch :: Cnstructor -> [Branch] -> Maybe Branch
findBranch = undefined