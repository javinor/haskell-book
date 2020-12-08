module Ch10_10_Exercises where

import Data.Function (on)

-- ===== Warm up & Review =====
-- Q1

stops = "pbtdkg"
vowels = "aeiou"

q1a = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- q1b = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']
q1b = filter (\(x,_,_) -> x == 'p') q1a


-- Q2

-- average length of a word in a sentance
seekritFunc x =
  div (sum (map length (words x)))
    (length (words x))


-- Q3

seekritFuncFractional x =
  fromIntegral (sum $ map length (words x)) 
    / fromIntegral (length (words x))

seekritFuncFractional' :: Fractional a => String -> a
seekritFuncFractional' x =
  on (/) fromIntegral numer denom
  where 
    numer = sum $ map length (words x)
    denom = length (words x)
  

-- ===== Rewriting functions using folds =====

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> acc || f x) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\e acc -> acc || e == x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' = any . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\x acc -> f x : acc) []
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f = foldr (\x acc -> f x ++ acc) []
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\acc y -> if (f acc y) == GT then acc else y) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\acc y -> if (f acc y) == LT then acc else y) x xs