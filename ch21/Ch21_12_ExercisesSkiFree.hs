module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = 
  S (n a) a 
  deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a)
        => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  -- foldMap :: (Foldable t, Monoid m) 
  --         => (a -> m) -> t a -> m
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  -- traverse :: (Traversable t, Applicative f) 
  --          => (a -> f b) -> t a -> f (t b)
  traverse f (S n a) = S <$> traverse f n <*> f a

main = do
  sample (arbitrary :: Gen (S [] Int))

  quickBatch $ functor (undefined :: S [] (Int, Int, String))
  quickBatch $ foldable (undefined :: S [] (Int, Int, String, Integer, Char))
  quickBatch $ traversable (undefined :: S [] (Int, Int, String))
