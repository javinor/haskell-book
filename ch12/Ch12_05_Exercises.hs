module Ch12_05_Exercises where

-- String Processing

notThe :: String -> Maybe String
notThe word = 
  if word == "the"
  then Nothing
  else Just word

replaceThe :: String -> String
replaceThe = unwords . replaceThe' . words
  where 
    replaceThe' :: [String] -> [String]
    replaceThe' [] = []
    replaceThe' (x:xs) = 
      let x' = case notThe x of 
               Nothing -> "a"
               Just word -> word
      in x' : replaceThe' xs
      

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBeforeVowel' . words
  where 
    countTheBeforeVowel' :: [String] -> Integer
    countTheBeforeVowel' [] = 0
    countTheBeforeVowel' (x:[]) = 0
    countTheBeforeVowel' (x:y:tl) = 
      counts + countTheBeforeVowel' (y:tl)
      where 
        counts = 
          if x == "the" && head y `elem` "aeiou"
          then 1
          else 0

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` "aeiou")


-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = 
  if length str < 2 * nVowels 
  then Nothing 
  else Just (Word' str)
  where nVowels = length $ filter (`elem` vowels) str


-- It's only Natural

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ integerToNat' n
  where integerToNat' 0 = Zero
        integerToNat' n = Succ $ integerToNat' (n - 1)
  

-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just a):xs) = a : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where go Nothing _ = Nothing
        go _ Nothing = Nothing
        go (Just x) (Just xs) = Just (x:xs)


-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where go (Left x) xs = x:xs
        go _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go (Right x) xs = x:xs
        go _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where go (Left x) (xs, ys) = (x:xs, ys)
        go (Right y) (xs, ys) = (xs, y:ys)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)


-- Write your own iterate and unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = 
  case f b of
    Nothing -> []
    Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))


-- Finally something other than a list!

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing -> Leaf
    Just (l, x, r) -> Node (unfold f l) x (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold go
  where go 0 = Nothing
        go n = Just (n - 1, n, n - 1)
