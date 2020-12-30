module Ch18_05_EitherMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

type SumQB = 
  Sum [Integer] (String, String, Integer)

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Sum a b) where
  arbitrary = 
    oneof [ First <$> arbitrary 
          , Second <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance Functor (Sum a) where
  fmap _ (First e)  = First e
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure = Second
  (First e) <*> _ = First e
  _ <*> (First e) = First e
  (Second f) <*> (Second x) = Second $ f x

instance Monad (Sum a) where
  return = pure
  (First e)  >>= _ = First e 
  (Second x) >>= f = f x

main :: IO () 
main = do
  quickBatch $ functor (undefined :: SumQB)
  quickBatch $ applicative (undefined :: SumQB)
  quickBatch $ monad (undefined :: SumQB)
