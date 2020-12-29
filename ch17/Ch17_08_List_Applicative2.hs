module Ch17_08_List_Applicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f = fold (Cons . f) Nil

instance Applicative List where
  pure x = Cons x Nil
  
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> vs =
    flatMap (\f -> fmap f vs) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency 
      [ (1, return Nil)
      , (3, Cons <$> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = 
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x $ fold f b xs

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = fold (append . f) Nil as


--------------------------------------- 
main :: IO () 
main = do
  let f = Cons (+1) $ Cons (*2) Nil
      v = Cons 1 $ Cons 2 Nil
  print $ f <*> v
  quickBatch $ functor (undefined :: List (String, String, Integer))
  quickBatch $ applicative (undefined :: List (String, String, Integer))