module PrettyPrinting where

import Ast
import Data.List (intercalate)

varName :: Variable -> String
varName (Variable x) = x

conName :: Cnstructor -> String
conName (Constructor c) = c

prettyPattern :: Pattern -> String
prettyPattern (PCon c []) = conName c
prettyPattern (PCon c vars) = conName c ++ " " ++ unwords (map varName vars)

prettyBranch :: Branch -> String
prettyBranch (Branch pat expr) = prettyPattern pat ++ " -> " ++ prettyExpr expr

prettyExpr :: Expr -> String
prettyExpr expr = case expr of
  Var v -> varName v
  Con c [] -> conName c
  Con c args -> conName c ++ "(" ++ intercalate ", " (map prettyExpr args) ++ ")"
  Fun f -> f
  Lam v e -> "\\" ++ varName v ++ " -> " ++ prettyExpr e
  App e1 e2 -> prettyApp e1 [e2]
  Case e branches -> 
      "case " ++ prettyExpr e ++ " of\n" ++
      "  " ++ intercalate "\n  " (map prettyBranch branches)
  where
    -- Collect nested applications
    collectApps :: Expr -> (Expr, [Expr])
    collectApps (App f arg) = 
        let (f', args) = collectApps f
        in (f', args ++ [arg])
    collectApps e = (e, [])
    

    prettyApp :: Expr -> [Expr] -> String
    prettyApp e1 e2s =
        let (f, args) = collectApps (foldl App e1 e2s)
            fStr = prettyAtomic f
            argsStr = unwords (map prettyAtomic args)
        in fStr ++ " " ++ argsStr


    
    isAtomic (Var _) = True
    isAtomic (Con _ []) = True
    isAtomic (Fun _) = True
    isAtomic _ = False
    
    prettyAtomic e
        | isAtomic e = prettyExpr e
        | otherwise = "(" ++ prettyExpr e ++ ")"

prettyDef :: Definition -> String
prettyDef (Definition name expr) = name ++ " = " ++ prettyExpr expr

prettyProgram :: Program -> String
prettyProgram (Program mainExpr defs) 
    | null defs = prettyExpr mainExpr
    | otherwise = prettyExpr mainExpr ++ " where\n" ++ 
                 indent 2 (intercalate "\n\n" (map prettyDef defs))
  where
    indent n = unlines . map (replicate n ' ' ++) . lines
    