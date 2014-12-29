module L' (leftFolds) where
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

propLast :: TestTree
propLast = testProperty "Last works"
           $ \l -> C.run l C.last == (mlast l :: Maybe Int)
  where mlast [] = Nothing
        mlast xs = Just (last xs)

propChunk :: TestTree
propChunk = testProperty "Chunk Works"
            $ \l -> C.run l (C.chunk id) == chunk (l :: [Int])
  where chunk [] = []
        chunk (x : xs) = takeWhile (== x) xs : chunk (dropWhile (== x) xs)

leftFolds :: TestTree
leftFolds = testGroup "Left Folds" [ propSum
                                   , propProduct
                                   , propCount
                                   , propNub
                                   , propLast]
