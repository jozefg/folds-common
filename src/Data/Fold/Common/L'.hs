module Data.Fold.Common.L' where
import Data.Fold
import Data.Fold.Internal
import Data.Monoid
import qualified Data.Set as S

-- | Sum of the inputs
sum :: Num a => L' a a
sum = L' id (+) 0

-- | Product of the input
product :: Num a => L' a a
product = L' id (*) 1

-- | Count the number of elements fed to a fold
count :: Enum e => L' a e
count = L' id (\c _ -> succ c) (toEnum 0)

-- | 'mappend' all the elements of a sequence together.
msum :: Monoid m => L' m m
msum = L' id mappend mempty

-- | Minimum of all inputs. If no inputs are supplied this returns
-- 'Nothing'.
minimum :: Ord a => L' a (Maybe a)
minimum = L' id comp Nothing
  where comp Nothing a  = Just a
        comp (Just b) a = Just (min a b)
-- | Maximum of all inputs. If no inputs are supplied this returns
-- 'Nothing'.
maximum :: Ord a => L' a (Maybe a)
maximum = L' id comp Nothing
  where comp Nothing a  = Just a
        comp (Just b) a = Just (max a b)

-- | De-duplicate all the inputs while preserving order. @O(n log(n))@
nub :: Ord a => L' a [a]
nub = L' (\(Pair' _ l) -> l) step (Pair' S.empty [])
  where step st@(Pair' s as) a | S.member a s = st
                               | otherwise = Pair' (S.insert a s) (a : as)

-- | De-duplicate all the inputs while preserving order. @O(n^2)@
slowNub :: Eq a => L' a [a]
slowNub = L' id step []
  where step as a | a `elem` as = as
                  | otherwise = a : as

-- | Collect all members into a @Set@.
intoSet :: Ord a => L' a (S.Set a)
intoSet = L' id (flip S.insert) S.empty

-- | Grab the last element inputted
last :: L' a (Maybe a)
last = L' id step Nothing
  where step Nothing = Just
        step x = const x

-- | Grab the nth element inputted
nth :: (Eq b, Num b) => b -> L' a (Maybe a)
nth b = L' (\(Pair' e _) -> maybe' Nothing Just e) step (Pair' Nothing' b)
  where step st@(Pair' (Just' _) _) _ = st
        step (Pair' _ 0) a = Pair' (Just' a) 0
        step (Pair' _ n) _ = Pair' Nothing' (n - 1)
