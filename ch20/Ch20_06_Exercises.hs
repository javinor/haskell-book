module Ch20_06_Exercises where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type  SBSIC = (String, Bool, String, Integer, Char)

-- Constant a b
data Constant a b =
  Constant b
  deriving (Eq, Show)

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq b => EqProp (Constant a b) where
  (=-=) = eq

instance Foldable (Constant a) where
  foldMap f (Constant a) = f a


-- Two a b
data Two a b =
  Two a b
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b)
         => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b


-- Three a b c
data Three a b c = 
  Three a b c
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c)
         => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary 
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c


-- Three' a b
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b)
         => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary 
                     <*> arbitrary
                     <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'


-- Four' a b
data Four' a b =
  Four' a b b b
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b)
         => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary 
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance Foldable (Four' a) where
  foldMap f (Four' a x y z) = f x <> f y <> f z


-- filterF
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF p xs =
  let f x = if p x then pure x else mempty 
  in foldMap f xs
  


---------------------
main :: IO ()
main = do
  putStrLn "\n\n=== Constant a b ==="
  quickBatch $ foldable (undefined :: Constant Bool SBSIC)

  putStrLn "\n\n=== Two a b ==="
  quickBatch $ foldable (undefined :: Two Bool SBSIC)

  putStrLn "\n\n=== Three a b c ==="
  quickBatch $ foldable (undefined :: Three Bool Char SBSIC)

  putStrLn "\n\n=== Three' a b ==="
  quickBatch $ foldable (undefined :: Three' Bool SBSIC)

  putStrLn "\n\n=== Four' a b ==="
  quickBatch $ foldable (undefined :: Four' Bool SBSIC)

  putStrLn "\n\n=== filterF ==="
  print $ (filterF (elem 'p') ["qwe", "asd", "zxc"] :: Maybe String)
  print $ (filterF (even . getSum) [Sum 1, Sum 2, Sum 3, Sum 4] :: Maybe (Sum Int))