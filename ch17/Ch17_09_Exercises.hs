module Ch17_08_Variation_on_Either where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type SSI = (String, String, Integer)

-- Pair a
data Pair a = 
  Pair a a 
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

type PairQB = Pair SSI

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)


-- Two a b
data Two a b = 
  Two a b 
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

type TwoQB = Two [Integer] SSI

instance Functor (Two a) where
  fmap f (Two a x) = Two a (f x)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a f) <*> (Two b v) = Two (a <> b) (f v)


-- Three a b c
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b 
         , Arbitrary c )
        => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

type ThreeQB = Three [Integer] All SSI

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' v) = 
    Three (a <> a') (b <> b') (f v)


-- Three' a b
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

type Three'QB = Three' Any SSI

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' b x y) =
    Three' (a <> b) (f x) (g y)


-- Four a b c d
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b 
         , Arbitrary c
         , Arbitrary d )
        => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ( Eq a, Eq b, Eq c, Eq d )
        => EqProp (Four a b c d) where
  (=-=) = eq

type FourQB = Four Any All (Sum Integer) SSI

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c f) <*> (Four a' b' c' v) = 
    Four (a <> a') (b <> b') (c <> c') (f v)


-- Four' a b
data Four' a b =
  Four' a a a b
  deriving (Eq, Show) 

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

type Four'QB = Four' [Integer] SSI

instance Functor (Four' a) where
  fmap f (Four' a b c x) = Four' a b c (f x)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a b c f) <*> (Four' a' b' c' v) =
    Four' (a <> a') (b <> b') (c <> c') (f v) 

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)


---------------------
main :: IO ()
main = do
  quickBatch $ functor (undefined :: PairQB)
  quickBatch $ applicative (undefined :: PairQB)

  quickBatch $ functor (undefined :: TwoQB)
  quickBatch $ applicative (undefined :: TwoQB)

  quickBatch $ functor (undefined :: ThreeQB)
  quickBatch $ applicative (undefined :: ThreeQB)

  quickBatch $ functor (undefined :: Three'QB)
  quickBatch $ applicative (undefined :: Three'QB)

  quickBatch $ functor (undefined :: FourQB)
  quickBatch $ applicative (undefined :: FourQB)

  quickBatch $ functor (undefined :: Four'QB)
  quickBatch $ applicative (undefined :: Four'QB)

  print $ combos stops vowels stops
