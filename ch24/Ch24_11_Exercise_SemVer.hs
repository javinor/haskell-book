module Ch24_11_Exercise_SemVer where

import Control.Applicative
import Test.Hspec
import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

parseNos :: Parser NumberOrString
parseNos = 
      (NOSI <$> (try $ integer <* notFollowedBy alphaNum))
  <|> (NOSS <$> some (alphaNum <|> char '-'))

parsePreRelease :: Parser [NumberOrString]
parsePreRelease =
  char '-' >> parseNos `sepBy1` char '.'

parseBuild :: Parser [NumberOrString]
parseBuild =
  char '+' >> parseNos `sepBy1` char '.'

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  pr <- parsePreRelease <|> pure []
  build <- parseBuild <|> pure []

  return $ SemVer major minor patch pr build

-- 
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  let psv = parseString parseSemVer mempty

  describe "SemVer Parsing" $ do
    it "can parse a simple SemVer" $ do
      let m = psv "2.1.1"
      print m
      maybeSuccess m `shouldBe` Just (SemVer 2 1 1 [] [])
    it "can parse a simple pre-release version" $ do
      let m = psv "1.0.0-x.7.z.92"
          expectedSemVer = 
            SemVer 1 0 0 [ NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
      print m
      maybeSuccess m `shouldBe` (Just expectedSemVer)
    it "can parse a pre-release version with mixed alphaNum identifiers" $ do
      let m = psv "1.0.0-x7.7x"
          expectedSemVer = 
            SemVer 1 0 0 [ NOSS "x7", NOSS "7x" ] []
      print m
      maybeSuccess m `shouldBe` (Just expectedSemVer)
    it "can parse a pre-release version with hiphens" $ do
      let m = psv "1.0.0-x-7"
          expectedSemVer = 
            SemVer 1 0 0 [ NOSS "x-7" ] []
      print m
      maybeSuccess m `shouldBe` (Just expectedSemVer)
    it "can parse integer build metadata" $ do
      let m = psv "1.0.0-gamma+002"
          expectedSemVer = 
            SemVer 1 0 0 [NOSS "gamma"] [NOSI 2]
      print m
      maybeSuccess m `shouldBe` (Just expectedSemVer)
    it "can parse simple dot-separated build metadata" $ do
      let m = psv "1.0.0-beta+sha.41af286.-17"
          expectedSemVer = 
            SemVer 1 0 0 [NOSS "beta"] [NOSS "sha", NOSS "41af286", NOSI (-17)]
      print m
      maybeSuccess m `shouldBe` (Just expectedSemVer)
    