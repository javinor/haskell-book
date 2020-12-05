module Ch09_12_Exercises where

import Data.Char

keepUpper :: String -> String
keepUpper = filter isUpper

capitalize :: String -> String
capitalize xs =
  case xs of
    [] -> []
    x : xs -> toUpper x : xs

allCaps :: String -> String
allCaps xs =
  case xs of
    [] -> []
    x : xs -> toUpper x : allCaps xs

capFirst :: String -> Char
capFirst xs = toUpper $ head xs

capFirst' :: String -> Char
capFirst' = toUpper . head