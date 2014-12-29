-- | This module exports a few common folds. Some of these are defined
-- as @L'@ folds if they cannot short-circuit. The rest are defined
-- as monoidal folds. Since you may want to combine a monoidal and
-- strict left fold, 'strictify' is an operation that drops laziness
-- and reassociates an 'M'. This may result in a slow down in
-- performance though.
--
-- For the classic example
--
-- > import           Control.Applicative
-- > import qualified Data.Fold.Common as C
-- >
-- > avg :: C.L' Double Double
-- > avg = (/) <$> C.sum <*> C.count
-- >
-- > main :: IO ()
-- > main = print $ C.run [1 .. 10000000] avg
--
-- This will run in constant memory as we'd hope.  In general the
-- rules for keeping memory usage low while using @folds@ are
--
-- * Don't try to consume a left fold lazily
-- * Don't try to consume a right fold strictly
-- * Never use '>>='
-- * Never use 'extend'
-- * Never use 'prefix' on right folds
-- * Never use 'postfix' on left folds
--
-- Since they destroy the primary advantage of folds in the first
-- place.
module Data.Fold.Common
       ( module Data.Fold
       , module Data.Fold.Common.L'
       , module Data.Fold.Common.M
       , module Data.Fold.Common.R) where
import Data.Fold
import Data.Fold.Common.L'
import Data.Fold.Common.M
import Data.Fold.Common.R
