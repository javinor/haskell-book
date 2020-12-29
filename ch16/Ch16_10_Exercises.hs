module Ch16_10_Exercises where

import Test.QuickCheck -- stack ghci --package QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) 
               => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) 
               => Fun a b -> Fun b c -> f a -> Bool
functorCompose' fab gbc x =
  (fmap g (fmap f x)) == (fmap (g . f) x)
  where f = applyFun fab
        g = applyFun gbc

-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityId = Identity Integer -> Bool
type IdentityComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Identity Integer 
  -> Bool


-- Pair a a
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x  <- arbitrary
    x' <- arbitrary
    return $ Pair x x'

type PairId = Pair Integer -> Bool
type PairComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Pair Integer
  -> Bool


-- Two a b
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoId = Two String Integer -> Bool
type TwoComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Two Integer Integer
  -> Bool


-- Three a b c
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) 
       => Arbitrary (Three a b c) where
  arbitrary = Three 
          <$> arbitrary 
          <*> arbitrary 
          <*> arbitrary

type ThreeId = Three Char Bool Integer -> Bool
type ThreeComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Three Char Bool Integer 
  -> Bool

-- Three' a b
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three'
          <$> arbitrary 
          <*> arbitrary 
          <*> arbitrary

type Three'Id = Three' Char Integer -> Bool
type Three'Comp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Three' Char Integer 
  -> Bool


-- Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d ) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

type FourId = Four Char Integer String Bool -> Bool
type FourComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Four Bool [Int] Char Integer 
  -> Bool


-- Four' a b
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four'
          <$> arbitrary 
          <*> arbitrary 
          <*> arbitrary
          <*> arbitrary

type Four'Id = Four' String Integer -> Bool
type Four'Comp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Four' Char Integer 
  -> Bool


-- Main
main :: IO ()
main = do
  quickCheck (functorIdentity :: IdentityId)
  quickCheck (functorCompose' :: IdentityComp)

  quickCheck (functorIdentity :: PairId)
  quickCheck (functorCompose' :: PairComp)

  quickCheck (functorIdentity :: TwoId)
  quickCheck (functorCompose' :: TwoComp)

  quickCheck (functorIdentity :: ThreeId)
  quickCheck (functorCompose' :: ThreeComp)

  quickCheck (functorIdentity :: Three'Id)
  quickCheck (functorCompose' :: Three'Comp)

  quickCheck (functorIdentity :: FourId)
  quickCheck (functorCompose' :: FourComp)

  quickCheck (functorIdentity :: Four'Id)
  quickCheck (functorCompose' :: Four'Comp)