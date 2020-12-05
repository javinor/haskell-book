module Ch09_12_Exercises where

import Data.Char

-- Data.Char

keepUpper :: String -> String
keepUpper = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = toUpper x : allCaps xs

capFirst :: String -> Char
capFirst xs = toUpper $ head xs

capFirst' :: String -> Char
capFirst' = toUpper . head


-- Writing your own standard functions

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:y:xs) = myMaximumBy f (z:xs)
  where z = case f x y of
              GT -> x
              _  -> y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:y:xs) = myMinimumBy f (z:xs)
  where z = case f x y of
              LT -> x
              _  -> y

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare