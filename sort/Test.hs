module Test where

import Algorithms

import System.Random
import Data.Time
import Control.Monad(replicateM)

import Data.List (sort)
-- Verify the correctness of algorithms
verifySort :: Ord a => ([a] -> [a]) -> [a] -> Bool
verifySort algo lst =
  algo lst == sort lst

generateRandomList :: Int -> IO [Int]
generateRandomList size = replicateM size randomIO

timeAlgorithm :: Ord a => String -> ([a] -> [a]) -> [a] -> IO ()
timeAlgorithm algorithmName algorithm unsortedList = do
    start <- getCurrentTime
    let _sorted = algorithm unsortedList
    end <- getCurrentTime
    let elapsedTime = diffUTCTime end start
    putStrLn $ algorithmName ++ " took " ++ show elapsedTime
