module Ch13_Exercises where

import Control.Monad
import Data.Char (toLower, isAlpha)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

isPalindrome :: String -> Bool
isPalindrome str = alphas == sahpla
  where alphas = (fmap toLower) . (filter isAlpha) $ str
        sahpla = reverse alphas


-- Q4

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter a name: "
  name <- getLine
  putStr "Please enter an age: "
  age <- getLine
  let age' = read age :: Age
  case mkPerson name age' of
    Right p -> 
      putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left err -> 
      putStrLn $ "Failed to get a person: " ++ show err