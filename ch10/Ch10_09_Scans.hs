module Ch10_09_Scans where

fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

fibsUnder100 = takeWhile (< 100) fibs

factorial = scanl (*) 1 [1..]