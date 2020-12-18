module Vigenere where

import Data.Char
import Caesar (shiftAlpha)

toShiftSize :: Char -> Int
toShiftSize c = ord c - base
  where base = if isUpper c then ord 'A' else ord 'a'

vigenere :: String -> String -> String
vigenere key xs = go key xs 0
  where 
    go _ [] _ = []
    go key (x:xs) index = 
      if x == ' '
      then x : go key xs index
      else 
        let x' = (shiftAlpha (toShiftSize $ key !! index) x)
            index' = (index + 1) `mod` length key
        in  x' : go key xs index'