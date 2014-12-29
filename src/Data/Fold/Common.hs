{-# LANGUAGE Safe #-}
-- | This module exports a few common folds. These are defined roughly
-- as follows.
--
--  * If it can't short-circuit, it's a @L'@
--  * If it can short-circuit and is associative, it's an @M@
--  * If it can short-circuit but isn't associative, it's an @R@
--
-- Since you may want to combine a monoidal and strict left fold,
-- 'strictify' is an operation that drops laziness and reassociates an
-- 'M'.
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
--  * Don't try to consume a left fold lazily
--  * Don't try to consume a right fold strictly
--  * Never use '>>='
--  * Never use 'extend'
--  * Never use 'prefix' on right folds
--  * Never use 'postfix' on left folds
--
-- Since they destroy the primary advantage of folds in the first
-- place.
module Data.Fold.Common
       ( module Data.Fold
         -- * Left Folds
       , sum
       , product
       , count
       , mconcat
       , minimum
       , maximum
       , nub
       , slowNub
       , intoSet
       , last
       , nth
         -- * Monoidal Folds
       , any
       , all
       , and
       , or
       , elem
       , notElem
       , find
       , head
       , null
       , strictify
         -- * Right Folds
       , intoList
       , take
       , drop
       , indexOf
       , chunk) where
import Prelude ()
import Data.Fold
import Data.Fold.Common.L'
import Data.Fold.Common.M
import Data.Fold.Common.R
