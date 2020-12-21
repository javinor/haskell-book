module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error "not a digit"

digits :: Int -> [Int]
digits n = go [] n
  where 
    go acc n =
      if n < 10 
      then n : acc
      else 
        let (q, r) = n `divMod` 10
        in go (r : acc) q

wordNumber :: Int -> String
wordNumber n = 
  concat $ intersperse "-" digitWords
  where digitWords = map digitToWord $ digits n