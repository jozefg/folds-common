-- | A series of common right folds. These tend to be list centric
-- since since lists provide such a lousy monoid. In particular this
-- means that a monoidal fold producing a list we almost always want
-- to associate right.
module Data.Fold.Common.R where
import Data.Fold
import Data.Fold.Internal

-- | An extremely boring fold. You can almost view this as an identity
-- fold across lists.
--
-- >>> run [1 .. 10] intoLists
-- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
intoList :: R a [a]
intoList = R id (:) []

-- | Take the first @n@ inputs to the fold. If less then @n@ inputs
-- are fed in total then take as many as possible.
--
-- >>> run [1 .. 10] (take 3)
-- [1, 2, 3]
--
-- >>> run [1, 2, 3] (take 100)
-- [1, 2, 3]
take :: (Eq b, Ord b, Num b) => b -> R a [a]
take b = R ($ b) step (\_ -> [])
  where step x xs n | n <= 0 = []
                    | otherwise = x : xs (n - 1)

-- | Drop the first @n@ items. If less then @n@ items are supplied
-- then return the empty list.
--
-- >>> run [1, 2, 3] (drop 1)
-- [2, 3]
--
-- >>> run [1, 2, 3] (drop 100)
-- []
drop :: (Eq b, Ord b, Num b) => b -> R a [a]
drop b = R ($ b) step (\_ -> [])
  where step x xs n | n <= 0 = x : xs 0
                    | otherwise = xs (n - 1)
        -- Note, this costs an integer comparison + thunks along the
        -- list. Is this really OK? It's still perfectly lazy at
        -- least.

-- | Find the first index for which a predicate holds.
--
-- >>> run [1, 2, 3, 4] (indexOf (== 4))
-- Just 3
--
-- >>> run [1, 2, 3, 4] (indexOf (> 4))
-- Nothing
indexOf :: Enum e => (a -> Bool) -> R a (Maybe e)
indexOf p = R (maybe' Nothing Just) step Nothing'
  where step a _ | p a = Just' (toEnum 0)
        step _ Nothing' = Nothing'
        step _ (Just' a) = Just' (succ a)
