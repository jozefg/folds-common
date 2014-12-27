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