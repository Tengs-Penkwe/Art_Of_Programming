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

-- Function to test all sorting algorithms for both correctness and performance
main :: IO ()
main = do
  -- Testing for correctness
  putStrLn "Correctness Tests:"
  listForCorrectness <- generateRandomList 10000
  forM_ sortingAlgorithms $ \(name, algo) -> do
    putStrLn $ name ++ ": " ++ show (verifySort algo listForCorrectness)
    
  forM_ listSizes $ \size -> do
    putStrLn $ "\nTesting with list size: " ++ show size

    -- Testing for performance
    putStrLn "Performance Tests:"
    listForPerformance <- generateRandomList size
    forM_ sortingAlgorithms $ \(name, algo) -> do
      timeAlgorithm name algo listForPerformance
