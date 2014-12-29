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

propAnd :: TestTree
propAnd = testProperty "And Works"
  $ \l -> C.run l C.and == and (l :: [Bool])

propOr :: TestTree
propOr = testProperty "Or Works"
  $ \l -> C.run l C.or == or (l :: [Bool])

propElem :: TestTree
propElem = testProperty "Elem Works"
          $ \l i -> C.run l (C.elem i) == elem i (l :: [Int])

propNotElem :: TestTree
propNotElem = testProperty "NotElem Works"
          $ \l i -> C.run l (C.notElem i) == notElem i (l :: [Int])


propFind :: TestTree
propFind = testProperty "Find Works"
           $ \l -> C.run l (C.find even) == find even (l :: [Int])

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
                                         , propAnd
                                         , propOr
                                         , propElem
                                         , propNotElem
                                         , propFind
                                         , propNull
                                         , propStrictify]
