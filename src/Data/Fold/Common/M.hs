module Data.Fold.Common.M where
import Prelude hiding (any, all)
import Data.Fold
import Data.Fold.Internal hiding (First)
import Data.Monoid

-- | Check that if predicate holds for any inputs to the fold.
any :: (a -> Bool) -> M a Bool
any p = M getAny (Any . p) (<>) (Any False)

-- | Check that if predicate holds for all inputs to the fold.
all :: (a -> Bool) -> M a Bool
all p = M getAll (All . p) (<>) (All False)

-- | Check whether all elements are 'True'
and :: M Bool Bool
and = all id

-- | Check whether any elements are 'True'
or :: M Bool Bool
or = any id

-- | Find the first element for which a predicate holds.
find :: (a -> Bool) -> M a (Maybe a)
find p = M getFirst to (<>) (First Nothing)
  where to a = First $ if p a then Just a else Nothing

-- | Find the first index for which a predicate holds.
indexOf :: Enum e => (a -> Bool) -> M a (Maybe e)
indexOf p = M (maybe' Nothing Just) to m Nothing'
  where to a = if p a then Just' (toEnum 0) else Nothing'
        m (Just' a) _ = Just' (succ a)
        m _ (Just' a) = Just' (succ a)
        m _ _ = Nothing'

-- | Grab the first inputted element
head :: M a (Maybe a)
head = M getFirst (First . Just) (<>) (First Nothing)

-- | Occasionally we want to use a short-circuiting fold with other,
-- nonlazy folds. This function drops laziness on the floor for a
-- 'L\'' fold.
strictify :: M a b -> L' a b
strictify (M p to m z) = L' p (\z a -> z `m` to a) z
