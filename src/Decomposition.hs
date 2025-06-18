module Decomposition where

import Ast

data Red
  = RFun String
  | RApp (Expr, Expr)
  | RCase (Expr, [Branch])
  deriving (Show, Eq)

data Context
  = Hole
  | AppCtx Context Expr
  | CaseCtx Context [Branch]
  deriving (Show, Eq)

type Decomposition = Either Expr (Context, Red) 
-- based on Unique Decomposition Property