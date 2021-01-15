module Ch24_11_Exercise_PhoneNums where

import Control.Applicative
import Data.Word
import Data.Char (intToDigit)
import Test.Hspec
import Text.Trifecta


-- IPv4

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

octetsToDecimal :: Integer -> Integer -> Integer -> Integer -> Integer
octetsToDecimal x1 x2 x3 x4 =
  x1 * 256 ^ 3 + x2 * 256 ^ 2 + x3 * 256 + x4

parseOctet :: Parser Integer
parseOctet = do
  x <- decimal
  if (0 <= x && x < 256)
  then return x
  else fail "Expected number to be octet - out of bounds"

parseIpAddress :: Parser IPAddress
parseIpAddress = do
  x1 <- parseOctet
  _ <- char '.'
  x2 <- parseOctet
  _ <- char '.'
  x3 <- parseOctet
  _ <- char '.'
  x4 <- parseOctet
  return $ IPAddress . fromInteger $ octetsToDecimal x1 x2 x3 x4


-- IPv6

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

parseIpV6 :: Parser IPAddress6
parseIpV6 = undefined

ipv6 :: Integer -> IPAddress6
ipv6 dec = 
  let (a,b) = dec `divMod` (2 ^ 64)
  in IPAddress6 (fromInteger a) (fromInteger b)

--
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  let pip = parseString parseIpAddress mempty
      pip6 = parseString parseIpV6 mempty

  describe "IPv4 Parsing" $ do
    it "parses 172.16.254.1 correctly" $ do
      let m = pip "172.16.254.1"
      print m
      maybeSuccess m `shouldBe` Just (IPAddress 2886794753)
    it "parses 204.120.0.15 correctly" $ do
      let m = pip "204.120.0.15"
      print m
      maybeSuccess m `shouldBe` Just (IPAddress 3430416399)

  describe "IPv6 Parsing" $ do
    it "parses 0:0:0:0:0:ffff:ac10:fe01 correctly" $ do
      let m = pip6 "0:0:0:0:0:ffff:ac10:fe01"
      print m
      maybeSuccess m `shouldBe` Just (ipv6 281473568538113)
    -- it "parses 0:0:0:0:0:ffff:cc78:f correctly" $ do
    --   let m = pip6 "0:0:0:0:0:ffff:cc78:f"
    --   print m
    --   maybeSuccess m `shouldBe` Just (IPAddress6 281474112159759)
    -- it "parses FE80:0000:0000:0000:0202:B3FF:FE1E:8329 correctly" $ do
    --   let m = pip6 "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
    --   print m
    --   maybeSuccess m `shouldBe` Just (IPAddress6 338288524927261089654163772891438416681)
    -- it "parses FE80::0202:B3FF:FE1E:8329 correctly" $ do
    --   let m = pip6 "FE80::0202:B3FF:FE1E:8329"
    --   print m
    --   maybeSuccess m `shouldBe` Just (IPAddress6 338288524927261089654163772891438416681)
    -- it "parses 2001:DB8::8:800:200C:417A correctly" $ do
    --   let m = pip6 "2001:DB8::8:800:200C:417A"
    --   print m
    --   maybeSuccess m `shouldBe` Just (IPAddress6 42540766411282592856906245548098208122)
