module Data.Fold.Common.L' where
import Data.Fold
import Data.Monoid

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
