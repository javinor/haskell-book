module Main where

import Caesar
import Vigenere

main :: IO ()
main = do
  putStrLn "Testing ciphers:"
  testCaesar
  testUnCaesar
  testVigenere

testCaesar :: IO ()
testCaesar = 
  if caesar 2 ['A' .. 'F'] == ['C' .. 'H']
  then putStrLn "caesar works!"
  else putStrLn "caesar's broken!"

testUnCaesar :: IO ()
testUnCaesar = 
  if unCaesar 2 ['C' .. 'H'] == ['A' .. 'F']
  then putStrLn "unCaesar works!"
  else putStrLn "unCaesar's broken!"

testVigenere :: IO ()
testVigenere = 
  if vigenere "ALLY" "MEET AT DAWN" == "MPPR AE OYWY"
  then putStrLn "vigenere works!"
  else putStrLn "vigenere's wrong!"

