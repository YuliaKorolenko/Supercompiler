module Driving.Spec where

import Test.Hspec
import Ast
import Decompose
import Decomposition
import Driving
import Dsl

spec :: Spec
spec = do
  describe "Test decompose" $ do
    it "correctly decompose program" $ do
        let lambdaExpr = Lam (Variable "x") (App (App (Fun "add") (Var (Variable "x"))) (Con (Constructor "Int") [Var (Variable "1")]))
        let argExpr = Con (Constructor "Int") [Var (Variable "5")]
        let appExpr = App lambdaExpr argExpr
        let result = decompose appExpr 
        result `shouldBe` Right (Hole, RApp (lambdaExpr, argExpr))

mainExpr :: Expr
mainExpr = (App (Fun "nrev") (Var (Variable "xs")))

specDriving :: Spec
specDriving = do
  describe "Driving Phase (Refactored)" $ do

    it "1. Drives a simple function application" $ do
      let idExpr = (App (Fun "id") (Con (Constructor "A") []))
      let idProgram = Program idExpr
                              [Definition "id" (Lam (Variable "x") (Var (Variable "x")))]
      let tree = driving idProgram idExpr
      let expected =
            Node (App (Fun "id") (Con (Constructor "A") []))
              [ Node (App (Lam (Variable "x") (v "x")) (Con (Constructor "A") []))
                  [ Node (Con (Constructor "A") [])
                    []
                  ]
              ]
      print tree
      tree `shouldBe` expected
    it "2. Drives a case expression on a constructor" $ do
      let expr = Case (Con (Constructor "Just") [Var (Variable "y")])
                      [ Branch (PCon (Constructor "Just") [Variable "x"]) (Var (Variable "x"))
                      , Branch (PCon (Constructor "Nothing") []) (Con (Constructor "Nil") [])
                      ]
      let dummyProgram = Program expr []
      let tree = driving dummyProgram expr
      let expected =
            Node expr
              [ Node (Var (Variable "y"))
                []
              ]
      tree `shouldBe` expected
    it "3. Drives a case expression on a variable (splits the path)" $ do
      let expr = Case (Var (Variable "z"))
                      [ Branch (PCon (Constructor "A") []) (Var (Variable "a"))
                      , Branch (PCon (Constructor "B") []) (Var (Variable "b"))
                      ]
      let dummyProgram = Program expr []
      let tree = driving dummyProgram expr
      let expected =
            Node expr
              [ Node (Var (Variable "a")) []
              , Node (Var (Variable "b")) []
              ]
      tree `shouldBe` expected