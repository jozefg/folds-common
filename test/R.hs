module Main where
import qualified Data.Fold.Common as C
import Data.List
import Test.Tasty
import Test.Tasty.QuickCheck


propIntoList :: TestTree
propIntoList = testProperty "intoList Works"
               $ \l -> C.run l C.intoList == (l :: [Int])

propTake :: TestTree
propTake = testProperty "Take Works"
           $ \l i -> C.run l (C.take i) == (take i l :: [Int])

propDrop :: TestTree
propDrop = testProperty "Drop Works"
           $ \l i -> C.run l (C.drop i) == (drop i l :: [Int])


propIndexOf :: TestTree
propIndexOf = testProperty "IndexOf Works"
              $ \l -> C.run l (C.indexOf even) == findIndex even (l :: [Int])

propChunk :: TestTree
propChunk = testProperty "Chunk Works"
            $ \l -> C.run l (C.chunk id) == chunk (l :: [Int])
  where chunk [] = []
        chunk (x : xs) =
          (x : takeWhile (== x) xs) : chunk (dropWhile (== x) xs)

propConcat :: TestTree
propConcat = testProperty "Concat Works"
              $ \l -> C.run l C.concat == concat (l :: [[Int]])

main :: IO ()
main = defaultMain $ testGroup "Right Folds" [ propIntoList
                                             , propTake
                                             , propDrop
                                             , propIndexOf
                                             , propChunk
                                             , propConcat]
