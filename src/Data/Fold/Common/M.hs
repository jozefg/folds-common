-- | A collection of right folds. These are all short circuiting and
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
all p = M getAll (All . p) (<>) (All False)

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

-- | Find the first index for which a predicate holds.
--
-- >>> run [1, 2, 3, 4] (indexOf (== 4))
-- Just 3
--
-- >>> run [1, 2, 3, 4] (indexOf (> 4))
-- Nothing
indexOf :: Enum e => (a -> Bool) -> M a (Maybe e)
indexOf p = M (maybe' Nothing Just) to m Nothing'
  where to a = if p a then Just' (toEnum 0) else Nothing'
        m (Just' a) _ = Just' (succ a)
        m _ (Just' a) = Just' (succ a)
        m _ _ = Nothing'

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
null = M id (const True) (||) False

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
