{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Decompose where

import Decomposition
import Ast

decompose :: Expr -> Decomposition
decompose e@(Var _) = Left e
decompose e@(Con _ _) = Left e
decompose e@(Lam _ _) = Left e
decompose (Fun funName) = Right (Hole, RFun funName)
decompose (App e1 e2) = case decompose e1 of
    Left (Lam v body) -> Right (Hole, RApp (Lam v body, e2))
    Left _ -> Right (AppCtx Hole e2, RApp (e1, e2))
    Right (ctx, redex) -> Right (AppCtx ctx e2, redex)
decompose (Case e branches) = case decompose e of
    Left (Con c args) -> Right (Hole, RCase (Con c args, branches))
    Left _ -> Right (CaseCtx Hole branches, RCase (e, branches))
    Right (ctx, redex) -> Right (CaseCtx ctx branches, redex)

