module Main where

import Caesar
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "What word would you like to encode? "
  word <- getLine
  putStr "What is the size of the shift? "
  size <- getLine
  let n = read size :: Int
  putStrLn $ "The encoded message is: " ++ caesar n word
