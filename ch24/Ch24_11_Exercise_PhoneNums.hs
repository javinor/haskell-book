module Ch24_11_Exercise_PhoneNums where

import Control.Applicative
import Test.Hspec
import Text.Trifecta


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange 
              LineNumber
  deriving (Eq, Show)

parseArea :: Parser Int
parseArea = 
  fmap read $ 
        try (count 3 digit)
    <|> try (between (char '(') (char ')') (count 3 digit))
    <|> (digit >> char '-' >> count 3 digit)

parseSeparator :: Parser (Maybe Char)
parseSeparator = 
  optional $ oneOf " -"

parsePhone :: Parser PhoneNumber
parsePhone = do
  area <- parseArea
  _ <- parseSeparator
  exch <- read <$> count 3 digit
  _ <- parseSeparator
  line <- read <$> count 4 digit
  return $ PhoneNumber area exch line


-- 
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  let ppn = parseString parsePhone mempty

  describe "Phone Number Parsing" $ do
    it "can parse a dashed phone number" $ do
      let m = ppn "123-456-7890"
      print m
      maybeSuccess m `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse an integer as a phone number" $ do
      let m = ppn "1234567890"
      print m
      maybeSuccess m `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse a phone number with parentheses" $ do
      let m = ppn "(123) 456-7890"
      print m
      maybeSuccess m `shouldBe` Just (PhoneNumber 123 456 7890)
    it "can parse a phone number with an international code " $ do
      let m = ppn "1-123-456-7890"
      print m
      maybeSuccess m `shouldBe` Just (PhoneNumber 123 456 7890)
    