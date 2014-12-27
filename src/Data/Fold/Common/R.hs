module Data.Fold.Common.R where
import Data.Fold
import Data.Fold.Internal

-- | Check that if predicate holds for any inputs to the fold.
any :: (a -> Bool) -> R a Bool
any p = R id ((||) . p) False

-- | Check that if predicate holds for all inputs to the fold.
all :: (a -> Bool) -> R a Bool
all p = R id ((&&) . p) True

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
