module Ch11_18_Exercisses_Vigenere where

import Data.Char

shiftAlpha :: Int -> Char -> Char
shiftAlpha n c = chr $ ((ord c - base + n) `mod` cycle) + base
  where
    cycle = ord 'z' - ord 'a' + 1
    base = if isUpper c then ord 'A' else ord 'a'

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


testVigenere :: IO ()
testVigenere = 
  if vigenere "ALLY" "MEET AT DAWN" == "MPPR AE OYWY"
  then putStrLn "it works!"
  else putStrLn "something's wrong!"