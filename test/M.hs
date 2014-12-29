module M (monoidFolds) where
import qualified Data.Fold.Common as C
import Data.List
import Test.Tasty
import Test.Tasty.QuickCheck

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

propNull :: TestTree
propNull = testProperty "Null Works"
          $ \l -> C.run l C.null == null (l :: [Int])

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
                                         , propNull
                                         , propStrictify]
