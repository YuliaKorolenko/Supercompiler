module Ast where

newtype Cnstructor
    = Constructor String
    deriving (Show, Eq)

newtype Variable
    = Variable String
    deriving (Show, Eq)

data Pattern
  = PCon Cnstructor [Variable]
  deriving (Show, Eq)

data Branch = Branch Pattern Expr
  deriving (Show, Eq)

data Expr
  = Var Variable         -- v     
  | Con Cnstructor [Expr]  -- c e1...ek 
  | Fun String         -- f 
  | Lam Variable Expr    -- λ-Abstraction 
  | App Expr Expr      -- Application 
  | Case Expr [Branch] -- Case Expression 
  deriving (Show, Eq)

data Definition = Definition String Expr
  deriving (Show, Eq)

data Program = Program Expr [Definition]
  deriving (Eq)

instance Show Program where
  show (Program mainExpr defs) =
    unlines $ (show mainExpr ++ " where") : map showDef defs
    where
      showDef (Definition name expr) = "  " ++ name ++ " = " ++ show expr