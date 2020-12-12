module Ch11_18_Exercises where

import Data.Char (toUpper)

-- As-patterns - Q1

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf t@(x:xs) (y:ys) = 
  isSubseqOf xs' ys
  where xs' = if x == y then xs else t

testIsSubseqOf :: IO ()
testIsSubseqOf = 
  if foldr (&&) True [ isSubseqOf "blah" "blahwoot"
                     , isSubseqOf "blah" "wootblah"
                     , isSubseqOf "blah" "wboloath"
                     , isSubseqOf "blah" "wootbla" == False
                     , isSubseqOf "blah" "halbwoot" == False
                     , isSubseqOf "blah" "blawhoot"
                     ]
  then putStrLn "isSubseqOf works!"
  else putStrLn "isSubseqOf is broken!"


-- As-patterns - Q2

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = 
  fmap toCapTuple (words sentence)
  where toCapTuple word@(x:xs) = (word, (toUpper x) : xs)

testCapitalizeWords :: IO ()
testCapitalizeWords =
  if capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]
  then putStrLn "capitalizeWords works!"
  else putStrLn "capitalizeWords is broken!"
 

-- Language Exercises

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord word@(x:xs) = toUpper x : xs

testCapitalizeWord :: IO ()
testCapitalizeWord =
  if capitalizeWord "chortle" == "Chortle" && capitalizeWord "Chortle" == "Chortle"
  then putStrLn "capitalizeWord works!"
  else putStrLn "capitalizeWord is broken!"


capitalizeParagraph :: String -> String
capitalizeParagraph p = unwords $ go (words p) True
  where go :: [String] -> Bool -> [String]
        go [] _ = []
        go (w:ws) toCap = (if toCap then capitalizeWord w else w) : go ws ('.' `elem` w)

testCapitalizeParagraph :: IO ()
testCapitalizeParagraph =
  if capitalizeParagraph "blah. woot ha." == "Blah. Woot ha."
  then putStrLn "capitalizeParagraph works!"
  else putStrLn "capitalizeParagraph is broken!"




