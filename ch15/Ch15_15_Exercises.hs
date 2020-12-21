module Ch15_12_QuickCheck where

-- import Control.Monad
import Data.Monoid
import Test.QuickCheck -- stack ghci --package QuickCheck

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool


-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

type IdentityAssoc = 
     Identity ([Int]) 
  -> Identity ([Int]) 
  -> Identity ([Int]) 
  -> Bool


-- Two a b
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two u v) = Two (x <> u) (y <> v)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoAssoc =
     Two (Sum Integer) All
  -> Two (Sum Integer) All
  -> Two (Sum Integer) All
  -> Bool


-- Three a b c
data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
  (<>) = const

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type ThreeAssoc =
     Three Any All (First Char)
  -> Three Any All (First Char)
  -> Three Any All (First Char)
  -> Bool


--  Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Semigroup (Four a b c d) where
  (<>) = flip const

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d ) 
         => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

type FourAssoc =
     Four Any All (First Char) (Last Double)
  -> Four Any All (First Char) (Last Double)
  -> Four Any All (First Char) (Last Double)
  -> Bool


-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _                             = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc =
     BoolConj
  -> BoolConj
  -> BoolConj
  -> Bool


-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj False) (BoolDisj False) = BoolDisj False
  (<>) _ _                               = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool


--  Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _       <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = 
    oneof [Fst <$> arbitrary, Snd <$> arbitrary]

type OrAssoc =
     Or (Product Char) All
  -> Or (Product Char) All
  -> Or (Product Char) All
  -> Bool


-- Combine
-- newtype Combine a b = Combine { unCombine :: (a -> b) }



-- <<<<< MAIN >>>>>
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
