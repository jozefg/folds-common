module Main where
import Test.Tasty
import M
import L'
import R

main :: IO ()
main = defaultMain $ testGroup "Folds" [leftFolds, monoidFolds, rightFolds]
