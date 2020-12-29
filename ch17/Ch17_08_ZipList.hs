module Ch17_08_ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l
          ys' = let (ZipList' l) = ys
                in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat
  (ZipList' fs) <*> (ZipList' vs) = 
    ZipList' $ zipWith ($) fs vs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

---------------------
main :: IO ()
main = do
  quickBatch $ applicative (undefined :: ZipList' (String, String, Integer))