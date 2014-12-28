-- | A collection of common left folds. Note that all of these are
-- strict and do not short circuit. These are useful for operations
-- that require inspecting the entire list to calculate the final
-- state.
module Data.Fold.Common.L' where
import Data.Fold
import Data.Fold.Internal
import Data.Monoid
import qualified Data.Set as S

-- | Sum of the inputs
--
-- >>> run [1 .. 10] sum
-- 55
sum :: Num a => L' a a
sum = L' id (+) 0

-- | Product of the input
--
-- >>> run [1 .. 10] product
-- 3628800
product :: Num a => L' a a
product = L' id (*) 1

-- | Count the number of elements fed to a fold
--
-- >>> run [1 .. 10] count
-- 10
--
-- Note: GHCi will default @Enum e@ to @()@. If you see
--
-- > *** Exception: Prelude.Enum.().succ: bad argument
--
-- You've been bitten by this.
count :: Enum e => L' a e
count = L' id (\c _ -> succ c) (toEnum 0)

-- | 'mappend' all the elements of a sequence together.
--
-- >>> run [[1, 2, 3, 4], [5, 6, 7, 8]] mconcat
-- [1, 2, 3, 4, 5, 6, 7, 8]
--
-- >>> run (map Sum [1, 2, 3, 4]) mconcat
-- Sum {getSum = 10}
mconcat :: Monoid m => L' m m
mconcat = L' id mappend mempty

-- | Minimum of all inputs. If no inputs are supplied this returns
-- 'Nothing'.
--
-- >>> run [1, 2, 3] minimum
-- 1
-- >>> run [1 ..] minimum
-- ... diverges ...
minimum :: Ord a => L' a (Maybe a)
minimum = L' id comp Nothing
  where comp Nothing a  = Just a
        comp (Just b) a = Just (min a b)

-- | Maximum of all inputs. If no inputs are supplied this returns
-- 'Nothing'.
--
-- >>> run [1, 2, 3] maximum
-- 3
--
-- >>> run [1 ..] maximum
-- ... diverges ...
maximum :: Ord a => L' a (Maybe a)
maximum = L' id comp Nothing
  where comp Nothing a  = Just a
        comp (Just b) a = Just (max a b)

-- | De-duplicate all the inputs while preserving order. @O(n log(n))@
--
-- >>> run (replicate 10 1 ++ replicate 10 2) nub
-- [1, 2]
--
-- >>> run [1, 2, 1] nub
-- [1, 2]
nub :: Ord a => L' a [a]
nub = L' (\(Pair' _ l) -> reverse l) step (Pair' S.empty [])
  where step st@(Pair' s as) a | S.member a s = st
                               | otherwise = Pair' (S.insert a s) (a : as)

-- | De-duplicate all the inputs while preserving
-- order. @O(n^2)@. This should be equivalent (but slower) then 'nub'
-- for 'Ord' types.
--
-- >>> run (replicate 10 1 ++ replicate 10 2) slowNub
-- [1, 2]
--
-- >>> run [1, 2, 1] slowNub
-- [1, 2]
slowNub :: Eq a => L' a [a]
slowNub = L' id step []
  where step as a | a `elem` as = as
                  | otherwise = a : as

-- | Collect all members into a @Set@.
--
-- >>> run [1 .. 10] intoSet
-- fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
intoSet :: Ord a => L' a (S.Set a)
intoSet = L' id (flip S.insert) S.empty

-- | Grab the last element inputted
--
-- >>> run [1 .. 10] last
-- Just 10
--
-- >>> run [] last
-- Nothing
last :: L' a (Maybe a)
last = L' id step Nothing
  where step _ = Just

-- | Grab the nth element inputted.
--
-- >>> run [1 .. 10] (nth 5)
-- Just 6
--
-- >>> run [1 .. 10] (nth 20)
-- Nothing
nth :: (Eq b, Num b) => b -> L' a (Maybe a)
nth b = L' (\(Pair' e _) -> maybe' Nothing Just e) step (Pair' Nothing' b)
  where step st@(Pair' (Just' _) _) _ = st
        step (Pair' _ 0) a = Pair' (Just' a) 0
        step (Pair' _ n) _ = Pair' Nothing' (n - 1)
