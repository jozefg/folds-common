module Data.Fold.Common.R where
import Prelude hiding (any, all)
import Data.Fold
import Data.Fold.Internal

-- | Check that if predicate holds for any inputs to the fold.
any :: (a -> Bool) -> R a Bool
any p = R id ((||) . p) False

-- | Check that if predicate holds for all inputs to the fold.
all :: (a -> Bool) -> R a Bool
all p = R id ((&&) . p) True

-- | Check whether all elements are 'True'
and :: R Bool Bool
and = all id

-- | Check whether any elements are 'True'
or :: R Bool Bool
or = any id

-- | Find the first element for which a predicate holds.
find :: (a -> Bool) -> R a (Maybe a)
find p = R id step Nothing
  where step a rest = if p a then Just a else rest

-- | Find the first index for which a predicate holds.
indexOf :: Enum e => (a -> Bool) -> R a (Maybe e)
indexOf p = R (maybe' Nothing Just) step Nothing'
  where step a rest | p a = Just' (toEnum 0)
                    | otherwise = case rest of
                                   Just' e -> Just' (succ e)
                                   Nothing'  -> Nothing'

-- | Grab the first inputted element
head :: R a (Maybe a)
head = R id (const . Just) Nothing

-- | Occasionally we want to use a short-circuiting fold with other,
-- nonlazy folds. This function drops laziness on the floor for a
-- 'L\'' fold.
strictify :: R a b -> L' a b
strictify (R p s z) = L' (\f -> p (f z)) (\f a -> s a . f) id
