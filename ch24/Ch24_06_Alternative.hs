{-# LANGUAGE QuasiQuotes #-}

module Ch24_06_Alternative where

import Ch24_04_ParsingFractions (virtuousFraction)
import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
      (Left <$> integer)
  <|> (Right <$> some letter)

parseNos' :: Parser NumberOrString
parseNos' =
  skipMany (oneOf "\n")
  >>
      (Left <$> integer)
  <|> (Right <$> some letter)

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

-- Excercise
data FracOrDec =
    Frac Rational
  | Dec Integer
  deriving (Eq, Show)

parseFracOrDec = 
      (Frac <$> try virtuousFraction)
  <|> (Dec <$> decimal)


main = do
  let p f i =
        parseString f mempty i

  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c

  print $ p (some parseNos') eitherOr

  putStrLn "\n\nFracOrDec exercise:"
  print $ p parseFracOrDec "10/3"
  print $ p parseFracOrDec "123"