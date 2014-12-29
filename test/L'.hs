module Main where
import qualified Data.Fold.Common as C
import Data.Monoid
import Data.List
import qualified Data.Set as S
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

propMconcat :: TestTree
propMconcat = testProperty "Mconcat Works"
              $ \l -> C.run (map Sum l) C.mconcat
                      == mconcat (map Sum l :: [Sum Int])

propMinimum :: TestTree
propMinimum = testProperty "Minimum Works"
          $ \l -> C.run l C.minimum == mminimum (l :: [Int])
  where mminimum [] = Nothing
        mminimum xs = Just (minimum xs)

propMaximum :: TestTree
propMaximum = testProperty "Maximum Works"
          $ \l -> C.run l C.maximum == mmaximum (l :: [Int])
  where mmaximum [] = Nothing
        mmaximum xs = Just (maximum xs)

propNub :: TestTree
propNub = testProperty "Nub Works"
          $ \l -> C.run l C.nub == nub (l :: [Int])

propSlowNub :: TestTree
propSlowNub = testProperty "SlowNub Works"
          $ \l -> C.run l C.slowNub == nub (l :: [Int])

propIntoSet :: TestTree
propIntoSet = testProperty "IntoSet Works"
          $ \l -> C.run l C.intoSet == S.fromList (l :: [Int])


propLast :: TestTree
propLast = testProperty "Last works"
           $ \l -> C.run l C.last == (mlast l :: Maybe Int)
  where mlast [] = Nothing
        mlast xs = Just (last xs)

main :: IO ()
main = defaultMain $ testGroup "Left Folds" [ propSum
                                            , propProduct
                                            , propCount
                                            , propMconcat
                                            , propMinimum
                                            , propMaximum
                                            , propNub
                                            , propSlowNub
                                            , propIntoSet
                                            , propLast]
