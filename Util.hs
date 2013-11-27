{- |
Module      : Util
Description : Miscellaneous utilities.

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2013-11-26

-}

module Util where

import Data.List

-- | Perform a Bayesian probability update to find P(A | Event).
probUpdate :: Double -> Double -> Double -> Double
probUpdate prior pEventGivenPrior pEvent =
  prior * pEventGivenPrior / pEvent

-- | Find the largest K elements of the list according to the
-- comparison function. The returned list is unsorted.
-- TODO: Computing length is O(n) and there may be some way to cache
-- the length.
-- TODO: Not tail-recursive.
-- TODO: Bad pivot value: slow if list is nearly [reverse-]sorted.
largestKBy :: (Eq a) => (a -> a -> Ordering) -> Int -> [a] -> [a]
largestKBy cmp k xs
  | k < length right = largestKBy cmp k right
  | k == length right = right
  | length right == 0 =
     -- only occurs if pivot is largest element
    pivot : (largestKBy cmp (k - 1) (delete pivot left))
  | otherwise = largestKBy cmp (k - length right) left ++ right
  where pivot = head xs
        (left, right) = partition ((/= LT) . cmp pivot) xs

largestK :: (Ord a) => Int -> [a] -> [a]
largestK = largestKBy compare
