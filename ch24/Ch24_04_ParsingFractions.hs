{-# LANGUAGE OverloadedStrings #-}
module Ch24_04_ParsingFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return $ numerator % denominator

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return $ numerator % denominator

yourFuncHere = do
  x <- integer
  eof
  return x

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  
  print $ parseString virtuousFraction mempty badFraction
  
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  -- print $ parseFraction' badFraction

  print $ parseString integer mempty "123abc"
  print $ parseString (integer >> eof) mempty "123abc"
  print $ parseString (integer >> eof) mempty "123"

  putStrLn "\n\nExercise: Units of Success"
  print $ parseString (yourFuncHere) mempty "123"
  print $ parseString (yourFuncHere) mempty "123abc"
