module Ch17_08_List_Applicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = 
    Cons (f x) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> vs = fmap f vs `append` (fs <*> vs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [ (1, return Nil)
              , (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = 
  Cons x $ xs `append` ys


main :: IO () 
main = do
  let f = Cons (+1) $ Cons (*2) Nil
      v = Cons 1 $ Cons 2 Nil
  print $ f <*> v
  quickBatch $ applicative (undefined :: List (String, String, Integer))