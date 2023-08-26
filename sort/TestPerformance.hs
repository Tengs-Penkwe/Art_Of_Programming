module TestPerformance where

import Algorithms

import System.Random
import Control.Monad
import Data.Time

-- Test performance of sorting algorithms
testPerformance :: IO ()
testPerformance = do
    putStrLn "Performance tests:"
    let listSize = 10000

    unsortedList <- generateRandomList listSize

    timeAlgorithm "Bubble Sort" bubbleSort unsortedList
    timeAlgorithm "Quick Sort" quickSort unsortedList
    -- Test other sorting algorithms

generateRandomList :: Int -> IO [Int]
generateRandomList size = replicateM size randomIO

timeAlgorithm :: String -> ([Int] -> [Int]) -> [Int] -> IO ()
timeAlgorithm algorithmName algorithm unsortedList = do
    start <- getCurrentTime
    let _sorted = algorithm unsortedList
    end <- getCurrentTime
    let elapsedTime = diffUTCTime end start
    putStrLn $ algorithmName ++ " took " ++ show elapsedTime
