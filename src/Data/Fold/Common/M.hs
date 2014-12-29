-- | A collection of monoidal folds. These are all short circuiting and
-- are designed to handle certain infinite cases properly. These are
-- useful for operations which don't require the full list to
-- calculate the output.
module Data.Fold.Common.M where
import Prelude hiding (any, all)
import Data.Fold
import Data.Fold.Internal hiding (First)
import Data.Monoid

-- | Check that if predicate holds for any inputs to the fold.
--
-- >>> run [1, 2, 3, 4] (any even)
-- True
--
-- >>> run [] (any $ const False)
-- False
any :: (a -> Bool) -> M a Bool
any p = M getAny (Any . p) (<>) (Any False)

-- | Check that if predicate holds for all inputs to the fold.
--
-- >>> run [1, 2, 3, 4] (all (< 6))
-- True
--
-- >>> run [1, 2, 3, 4] (all (> 1))
-- False
all :: (a -> Bool) -> M a Bool
all p = M getAll (All . p) (<>) (All True)

-- | Check whether all elements are 'True'.
--
-- >>> run (repeat False) and
-- False
--
-- >>> run (repeat True) and
-- ... diverges ...
and :: M Bool Bool
and = all id

-- | Check whether any elements are 'True'.
--
-- >>> run (True : repeat False) or
-- True
-- >>> run (repeat False) or
-- ... diverges ...
or :: M Bool Bool
or = any id

-- | Check whether an element is fed into the fold.
--
-- >>> run [1, 2, 3] (elem 3)
-- True
--
-- >>> run [] (elem 1)
-- False
elem :: Eq a => a -> M a Bool
elem a = any (== a)

-- | Check whther an element isn't fed into the fold.
-- >>> run [1, 2, 3] (notElem 3)
-- False
--
-- >>> run [] (notElem 1)
-- True
notElem :: Eq a => a -> M a Bool
notElem a = all (/= a)

-- | Find the first element for which a predicate holds.
--
-- >>> run [1, 2, 3, 4] (find even)
-- Just 2
--
-- >>> run [1, 2, 3, 4] (find (> 4))
-- Nothing
find :: (a -> Bool) -> M a (Maybe a)
find p = M getFirst to (<>) (First Nothing)
  where to a = First $ if p a then Just a else Nothing

-- | Grab the first inputted element.
--
-- >>> run [1 ..] head
-- Just 1
--
-- >>> run [] head
-- Nothing
head :: M a (Maybe a)
head = M getFirst (First . Just) (<>) (First Nothing)

-- | Check whether a fold was fed any elements.
--
-- >>> run [] null
-- True
--
-- >>> run [1..] null
-- False
null :: M a Bool
null = M id (const False) (&&) True

-- | Occasionally we want to use a short-circuiting fold with other,
-- nonlazy folds. This function drops laziness on the floor for a @L'@
-- fold. This is dangerous because it can potentially effect
-- termination behavior.
--
-- >>> run (repeat False) and
-- False
--
-- >>> run (repeat False) (strictify and)
-- ... diverges ...
--
-- This means it is only advisable to use when combining a monoidal
-- fold with something that requires left folding.
--
-- >>> run [1.0, 2, 3, 4] $ (/) <$> strictify head <*> maximum
-- 0.25
strictify :: M a b -> L' a b
strictify (M p to m z) = L' p (\z a -> z `m` to a) z
