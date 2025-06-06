module Main where

import Ast
import Driving
import Dsl
import PrettyPrinting 

-- | The list reversal example from Fig. 2
nrevExample :: Program
nrevExample = Program
              (App (Fun "nrev") (c ":" [v "a", v "b"]))
              [ Definition "nrev"
                        (Lam (Variable "xs")
                            (Case (v "xs")
                                    [ branch "[]" [] (c "[]" [])
                                    , branch ":"  ["x'", "xs'"]
                                              (App (App (Fun "app") (App (Fun "nrev") (v "xs'")))
                                                    ((Con $ Constructor ":") [v "x'", c "[]" []]))]
                            )
                        )
               , Definition "app" (Lam (Variable "xs") (Lam (Variable "ys")
                                        (Case (v "xs")
                                            [ branch "[]" [] (v "ys")
                                            , branch ":" ["x'","xs'"]
                                                    (c ":" [v "x'", App (App (Fun "app") (v "xs'")) (v "ys")])])))
              ]

outer :: Expr
outer = Case (Var (Variable "xs")) [ 
            Branch (PCon (Constructor "[]") []) (v "smth"), 
            Branch (PCon (Constructor ":") [Variable "x'", Variable "xs'"]) (v "inner")] 

test_driving_nrev :: IO ()
test_driving_nrev = do
    let Program mainExpr _ = nrevExample
    let driven = drive nrevExample mainExpr

    case driven of
        Case ((Con (Constructor ":") [Var (Variable "a"), Var (Variable "b")])) [ 
            Branch (PCon (Constructor "[]") []) _, 
            Branch (PCon (Constructor ":") [Variable "x'", Variable "xs'"]) inner 
            ] -> 
                case inner of
                    App (App (Fun "app") (App (Fun "nrev") (Var (Variable "xs'")))) _ -> 
                        putStrLn "Test 1 Passed: Structure matches after driving"
                    _ -> putStrLn $ "Test 1 Failed: Inner expr mismatch - " ++ show inner
        _ -> putStrLn $ "Test 1 Failed: Unexpected result - " ++ prettyExpr driven 
    
    putStrLn $ prettyExpr driven

main :: IO ()
main = do
    test_driving_nrev
