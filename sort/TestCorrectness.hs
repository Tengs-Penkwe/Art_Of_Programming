module TestCorrectness where

import Algorithms

-- Test correctness of sorting algorithms
testCorrectness :: IO ()
testCorrectness = do
    putStrLn "Correctness tests:"
    let unsortedList = [4, 2, 9, 1, 5]
    let sortedList = [1, 2, 4, 5, 9]

    testAlgorithm "Bubble Sort" bubbleSort unsortedList sortedList
    -- testAlgorithm "Quick Sort" quickSort unsortedList sortedList
    -- Test other sorting algorithms

testAlgorithm :: (Eq a, Show a) => String -> ([a] -> [a]) -> [a] -> [a] -> IO ()
testAlgorithm algorithmName algorithm unsorted sorted =
    if algorithm unsorted == sorted
        then putStrLn $ algorithmName ++ " test passed."
        else putStrLn $ algorithmName ++ " test failed."
