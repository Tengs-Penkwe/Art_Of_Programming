module Main where

import Algorithms
import Test
import Control.Monad(forM_)

-- List of sorting algorithms to test
sortingAlgorithms :: [(String, [Int] -> [Int])]
sortingAlgorithms = 
  [ ("Counting Sort", countSort)
  , ("Bubble Sort", bubbleSort)
  -- add new sorting algorithms here
  ]

-- List sizes to test
listSizes :: [Int]
listSizes = [10000, 100000, 1000000, 10000000]

-- Define some edge case lists
edgeCases :: [[Int]]
edgeCases =
  [ []
  , [1]
  , [2, 2, 2, 2, 2]
  , [1..10]
  , [10, 9..1]
  , take 1000 $ cycle [1,2,3]  -- Repeating pattern
  , [100, 99..0] ++ [1..100]    -- Decreasing then increasing
  , replicate 100 0 ++ [1..100] -- Lots of zeros then ascending
  , [1..50] ++ [50, 49..1]      -- Ascending then descending
  , [2, 4..1000]                -- Even numbers
  , [1, 3..999]                 -- Odd numbers
  , take 500 $ cycle [0,1]      -- Alternating 0 and 1
  , [1000, 999..1]              -- Strictly descending
  , [1, 1000..2000]             -- Big jump in numbers
  , [1..500] ++ [500, 499..1]   -- Ascending and then descending
  , replicate 1000 0            -- All zeros
  , replicate 1000 1000         -- All same non-zero
  , [1000, 999..900] ++ [800, 801..1000] -- Descending and ascending
  , take 1000 $ cycle [1000, -1000] -- Alternating large numbers
  , [x * (-1)^x | x <- [1..1000]] -- Alternating sign
  , [1] ++ replicate 999 0      -- Mostly zeros
  , [1000, 999..500] ++ [500, 501..1000] -- Descending and ascending
  , [x + y | x <- [1..50], y <- [0, 50..950]] -- Multiple sequences
  , [x * 100 | x <- [1..1000]]  -- Multiples of 100
  ]

-- Function to test all sorting algorithms for both correctness and performance
main :: IO ()
main = do
  -- Testing for correctness
  putStrLn "Correctness Tests:"
  listForCorrectness <- generateRandomList 10000
  forM_ sortingAlgorithms $ \(name, algo) -> do
    let randomTestResult = verifySort algo listForCorrectness
    let edgeCaseTestResults = map (verifySort algo) edgeCases
    let allTestResults = all id (randomTestResult : edgeCaseTestResults)
    putStrLn $ name ++ ": " ++ show allTestResults
    
  forM_ listSizes $ \size -> do
    putStrLn $ "\nTesting with list size: " ++ show size

    -- Testing for performance
    putStrLn "Performance Tests:"
    listForPerformance <- generateRandomList size
    forM_ sortingAlgorithms $ \(name, algo) -> do
      timeAlgorithm name algo listForPerformance
