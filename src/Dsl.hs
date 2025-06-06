module Dsl where

import Ast

v :: String -> Expr
v x = Var (Variable x)

c :: String -> [Expr] -> Expr
c name = Con (Constructor name)

branch :: String -> [String] -> Expr -> Branch
branch con vars = Branch (PCon (Constructor con) (map Variable vars))