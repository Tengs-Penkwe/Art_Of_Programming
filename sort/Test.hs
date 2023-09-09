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

timeAlgorithm :: Ord a => ([a] -> [a]) -> [a] -> IO ()
timeAlgorithm algorithm unsortedList = do
    start <- getCurrentTime
    let sorted = algorithm unsortedList
    sorted `seq` return ()      -- Ensure this part is executed in sequence so we can get the right result.
    end <- getCurrentTime
    let elapsedTime = diffUTCTime end start
    putStrLn $ show (length unsortedList) ++ "\t took " ++ show elapsedTime
