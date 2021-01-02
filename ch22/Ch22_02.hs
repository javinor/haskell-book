module Ch22_02 where

import Control.Applicative
import Data.Char

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop

bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop

-- Warm up
cap :: [Char] -> [Char]
cap = fmap toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) cap rev

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
  s <- cap
  t <- rev
  return (s,t)

tupled''' :: [Char] -> ([Char], [Char])
tupled''' = 
  cap >>= \s -> 
    rev >>= \t -> 
      return (s, t)
    