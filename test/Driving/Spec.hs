module Driving.Spec where

import Test.Hspec
import Ast
import Decompose
import Decomposition

spec :: Spec
spec = do
  describe "Test decompose" $ do
    it "correctly decompose program" $ do
        let lambdaExpr = Lam (Variable "x") (App (App (Fun "add") (Var (Variable "x"))) (Con (Constructor "Int") [Var (Variable "1")]))
        let argExpr = Con (Constructor "Int") [Var (Variable "5")]
        let appExpr = App lambdaExpr argExpr
        let result = decompose appExpr 
        result `shouldBe` Right (Hole, RApp (lambdaExpr, argExpr))