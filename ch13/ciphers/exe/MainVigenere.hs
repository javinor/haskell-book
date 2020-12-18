module Main where

import System.IO
import Vigenere

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "What word would you like to encode? "
  word <- getLine
  putStr "What is the encryption keyword? "
  key <- getLine
  putStrLn $ "The encoded message is: " ++ vigenere key word
