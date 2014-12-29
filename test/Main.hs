module Main where
import Test.Tasty
import M
import L'

main :: IO ()
main = defaultMain $ testGroup "Folds" [leftFolds, monoidFolds]
