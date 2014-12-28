module Main where
import qualified Data.Fold.Common as C
import Data.List
import Test.Tasty
import Test.Tasty.QuickCheck


propSum :: TestTree
propSum = testProperty "Sum Works"
          $ \l -> C.run l C.sum == (sum l :: Int)

propProduct :: TestTree
propProduct = testProperty "Product Works"
              $ \l -> C.run l C.product == (product l :: Int)

propCount :: TestTree
propCount = testProperty "Count Works"
            $ \l -> C.run l C.count == length (l :: [Int])

propNub :: TestTree
propNub = testProperty "Nub Works"
          $ \l -> C.run l C.nub == nub (l :: [Int])

leftFolds :: TestTree
leftFolds = testGroup "Left Folds" [ propSum
                                   , propProduct
                                   , propCount
                                   , propNub]

propAny :: TestTree
propAny = testProperty "Any Works"
          $ \l -> C.run l (C.any even) == any even (l :: [Int])

propAll :: TestTree
propAll = testProperty "All Works"
          $ \l -> C.run l (C.all even) == all even (l :: [Int])

propFind :: TestTree
propFind = testProperty "Find Works"
           $ \l -> C.run l (C.find even) == find even (l :: [Int])

propIndexOf :: TestTree
propIndexOf = testProperty "IndexOf Works"
              $ \l -> C.run l (C.indexOf even) == findIndex even (l :: [Int])

propStrictify :: TestTree
propStrictify = testGroup "Strictify Works" [ s "any" $ C.any even
                                            , s "all" $ C.all even
                                            , s "find" $ C.find even]
  where s name f =
          testProperty name
          $ \l -> C.run (l :: [Int]) f == C.run l (C.strictify f)


monoidFolds :: TestTree
monoidFolds = testGroup "Monoidal Folds" [ propAny
                                         , propAll
                                         , propFind
                                         , propIndexOf
                                         , propStrictify]

main :: IO ()
main = defaultMain $ testGroup "Folds" [leftFolds]
