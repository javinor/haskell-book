module Ch24_11_Exercise_PositiveInteger where

import Control.Applicative
import Test.Hspec
import Text.Trifecta


parseDigit' :: Parser Char
parseDigit' = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = do 
  negs <- many (char '-')
  digits <- some parseDigit'
  return $ read (negs ++ digits)

-- 
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  let ps p = parseString p mempty

  describe "Positive Integer Parsing" $ do
    it "can parse a digit" $ do
      let m = ps parseDigit' "123"
      print m
      maybeSuccess m `shouldBe` Just '1'
    it "can parse a positive integer" $ do
      let m = ps base10Integer "123"
      print m
      maybeSuccess m `shouldBe` Just 123
    it "can parse a negative integer" $ do
      let m = ps base10Integer "-123abc"
      print m
      maybeSuccess m `shouldBe` Just (-123)
    