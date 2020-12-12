module Ch11_18_Exercises_Phone where

import Data.Char (isAlpha, isUpper, toLower)
import Data.Function (on)
import Data.List (elemIndex, group, maximumBy, sort)

type CapsButton = Char
data Button = Button Char [Char] deriving (Show, Eq)
data DaPhone = DaPhone CapsButton [Button]

------------------------------
-- | 1      | 2 ABC | 3 DEF  |
------------------------------
-- | 4 GHI  | 5 JKL | 6 MNO  |
------------------------------
-- | 7 PQRS | 8 TUV | 9 WXYZ |
------------------------------
-- | * ^    | 0 + _ | # .,   |
------------------------------

defaultPhone :: DaPhone
defaultPhone = 
  DaPhone '*' [ Button '2' "abc"
              , Button '3' "def"
              , Button '4' "ghi"
              , Button '5' "jkl"
              , Button '6' "mno"
              , Button '7' "prqs"
              , Button '8' "tuv"
              , Button '9' "wxyz"
              , Button '0' " "
              , Button '#' ".,"
              ]

presses :: Button -> Char -> Maybe Int
presses (Button b xs) c =
  if (c == b) 
  then Just (length xs + 1) 
  else fmap (+1) $ elemIndex c xs 

digitPresses :: Char -> [Button] -> [(Digit, Presses)]
digitPresses char [] = []
digitPresses char (btn@(Button digit chars):btns) =
  case presses btn char of
    Nothing -> digitPresses char btns
    Just n -> [(digit, n)]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone caps buttons) char =
  if isUpper char
  then (caps, 1) : digitPresses'
  else digitPresses'
  where digitPresses' = digitPresses (toLower char) buttons
    
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone = concat . fmap (reverseTaps daPhone)


-- Q3

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0


-- Q4 

freq :: [[a]] -> [(a, Int)]
freq [] = []
freq ([]:xss) = freq xss
freq (xs@(x:_):xss) = (x, length xs) : freq xss 

mostPopularLetter :: String -> Char
mostPopularLetter = fst . maximumBy (compare `on` snd) . freq . group . sort . fmap toLower

mostPopularNonSpace :: String -> Char
mostPopularNonSpace = fst . maximumBy (compare `on` snd) . freq . group . sort . fmap toLower . filter (/= ' ')

costOfMostPopularNonSpace :: String -> Int
costOfMostPopularNonSpace msg = 
  let letter = mostPopularNonSpace msg
      chars = filter (\x -> toLower x == letter) msg
  in  fingerTaps $ cellPhonesDead defaultPhone chars

-- Q5

coolestLtr :: [String] -> Char
coolestLtr = mostPopularNonSpace . concat

coolestWord :: [String] -> String
coolestWord = head . maximumBy (compare `on` length) . group . sort . concat . (fmap words) . fmap (filter (\c -> c /= '.' || c /= ','))