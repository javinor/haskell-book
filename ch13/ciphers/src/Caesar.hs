module Caesar where

import Data.Char

shiftAlpha :: Int -> Char -> Char
shiftAlpha n c = chr $ ((ord c - base + n) `mod` cycle) + base
  where
    cycle = ord 'z' - ord 'a' + 1
    base = if isUpper c then ord 'A' else ord 'a'

caesar :: Int -> String -> String
caesar n xs = fmap (shiftAlpha n) xs

unCaesar :: Int -> String -> String
unCaesar n xs = fmap (shiftAlpha (-n)) xs