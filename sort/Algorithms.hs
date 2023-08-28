module Algorithms where

import Data.List (sortOn)
-- Counting Sort
countSort :: Ord a => [a] -> [a]
countSort arr =
  let counts = [length [y | y <- arr, y < x] | x <- arr]
  in map snd . sortOn fst $ zip counts arr

-- Bubble Sort algorithm
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = go xs (length xs)
    where
        go :: Ord a => [a] -> Int -> [a]
        go xs 0 = xs
        go xs n = go (bubble xs) (n - 1)

        bubble :: Ord a => [a] -> [a]
        bubble [] = []
        bubble [x] = [x]
        bubble (x1:x2:xs)
            | x1 > x2   = x2 : bubble (x1:xs)
            | otherwise = x1 : bubble (x2:xs)
