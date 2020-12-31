module Ch20_05_LibraryFunctions where

import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs

product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct $ foldMap Product xs

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem' e xs = foldr (\x acc -> acc || e == x) False xs
elem' e xs = getAny $ foldMap (Any . (== e)) xs

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = 
  foldr mmin Nothing xs
    where mmin x Nothing = Just x
          mmin x (Just y) = Just $ min x y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs =
  foldr mmax Nothing xs
    where mmax x Nothing = Just x
          mmax x (Just y) = Just $ max x y

null' :: Foldable t => t a -> Bool
null' ta = getAll $ foldMap (const (All False)) ta

length' :: (Foldable t) => t a -> Int
length' xs = getSum $ foldMap (const $ Sum 1) xs

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr ((<>) . f) mempty