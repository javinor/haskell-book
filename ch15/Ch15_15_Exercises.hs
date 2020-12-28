module Ch15_15_Exercises where

import Data.Monoid
import Test.QuickCheck hiding (Success, Failure) 
-- stack ghci --package QuickCheck

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool


-- Identity a
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

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

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

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

instance Monoid BoolConj where
  mempty = BoolConj True

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

instance Monoid BoolDisj where
  mempty = BoolDisj False

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
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CombineAssoc =
     Fun Integer [Char]
  -> Fun Integer [Char]
  -> Fun Integer [Char]
  -> Integer
  -> Bool

semigroupAssocCombine :: (Eq b, Semigroup b) => Fun a b -> Fun a b -> Fun a b -> a -> Bool
semigroupAssocCombine f g h x =
  unCombine ((cf <> cg) <> ch) x == unCombine (cf <> (cg <> ch)) x
  where 
    cf = Combine $ applyFun f
    cg = Combine $ applyFun g 
    ch = Combine $ applyFun h

monoidLeftIdentityCombine :: (Eq b, Monoid b) => Fun a b -> a -> Bool
monoidLeftIdentityCombine f x =
  unCombine (mempty <> cf) x == applyFun f x
  where
    cf = Combine $ applyFun f


monoidRightIdentityCombine :: (Eq b, Monoid b) => Fun a b -> a -> Bool
monoidRightIdentityCombine f x =
  unCombine (cf <> mempty) x == applyFun f x
  where
    cf = Combine $ applyFun f


-- Comp a
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

type CompAssoc =
     Fun String String
  -> Fun String String
  -> Fun String String
  -> String
  -> Bool

semigroupAssocComp :: Eq a => Fun a a -> Fun a a -> Fun a a -> a -> Bool
semigroupAssocComp f g h x =
  unComp ((cf <> cg) <> ch) x == unComp (cf <> (cg <> ch)) x
  where 
    cf = Comp $ applyFun f
    cg = Comp $ applyFun g 
    ch = Comp $ applyFun h

monoidLeftIdentityComp :: (Eq a) => Fun a a -> a -> Bool
monoidLeftIdentityComp f x =
  unComp (mempty <> cf) x == applyFun f x
  where
    cf = Comp $ applyFun f


monoidRightIdentityComp :: (Eq a) => Fun a a -> a -> Bool
monoidRightIdentityComp f x =
  unComp (cf <> mempty) x == applyFun f x
  where
    cf = Comp $ applyFun f


-- Validation
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure x) <> (Failure y) = Failure (x <> y)
  (Success x) <> _           = Success x
  _           <> sy          = sy

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ Failure a
          , return $ Success b ]

type ValidationAssoc =
     Validation String Integer
  -> Validation String Integer
  -> Validation String Integer
  -> Bool


-- Mem s a
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = 
    Mem $ \s -> let (a1, s1) = f s
                    (a2, s2) = g s1
                in (a1 <> a2, s2)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

type MemAssoc =
     Blind (Mem Integer String)
  -> Blind (Mem Integer String)
  -> Blind (Mem Integer String)
  -> Integer
  -> Bool


semigroupAssocMem :: (Eq s, Eq a, Semigroup a) 
                  => Blind (Mem s a) 
                  -> Blind (Mem s a) 
                  -> Blind (Mem s a) 
                  -> s 
                  -> Bool
semigroupAssocMem (Blind m) (Blind m') (Blind m'') s =
  runMem ((m <> m') <> m'') s == runMem (m <> (m' <> m'')) s

monoidLeftIdentityMem :: (Eq s, Eq a, Monoid a) => Blind (Mem s a) -> s -> Bool
monoidLeftIdentityMem (Blind m) s =
  runMem (mempty <> m) s == runMem m s

monoidRightIdentityMem :: (Eq s, Eq a, Monoid a) => Blind (Mem s a) -> s -> Bool
monoidRightIdentityMem (Blind m) s =
  runMem (m <> mempty) s == runMem m s


-- <<<<< MAIN >>>>>
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two Any All -> Bool)
  quickCheck (monoidRightIdentity :: Two Any All -> Bool)

  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  quickCheck (semigroupAssoc :: OrAssoc)

  quickCheck (semigroupAssocCombine :: CombineAssoc)
  quickCheck (monoidLeftIdentityCombine :: Fun Integer [Char] -> Integer -> Bool)
  quickCheck (monoidRightIdentityCombine :: Fun Integer [Char] -> Integer -> Bool)

  quickCheck (semigroupAssocComp :: CompAssoc)
  quickCheck (monoidLeftIdentityComp :: Fun String String -> String -> Bool)
  quickCheck (monoidRightIdentityComp :: Fun String String -> String -> Bool)

  quickCheck (semigroupAssoc :: ValidationAssoc)

  let f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0

  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

  quickCheck (semigroupAssocMem :: MemAssoc)
  quickCheck (monoidLeftIdentityMem :: Blind (Mem Integer String) -> Integer -> Bool)
  quickCheck (monoidRightIdentityMem :: Blind (Mem Integer String) -> Integer -> Bool)
