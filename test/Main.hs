module Main (main) where

import Test.Hspec

import Driving.Spec

main :: IO ()
main = do
    hspec spec