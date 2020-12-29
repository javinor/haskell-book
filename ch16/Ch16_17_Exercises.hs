{-# LANGUAGE FlexibleInstances #-} -- for: Flip k a

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


-- Quant a b
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor x) = Bloor (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary =
    oneof [ return $ Finance
          , Desk <$> arbitrary
          , Bloor <$> arbitrary ]

type QuantId = Quant Integer String -> Bool
type QuantComp =
     Fun Integer String 
  -> Fun String [Bool] 
  -> Quant Char Integer 
  -> Bool


-- K a b
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

type KId = K Integer String -> Bool
type KComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> K (Maybe Char) Integer
  -> Bool


-- Flip f a b
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip $ K' (f x)

instance Arbitrary a => Arbitrary (K' a b) where
  arbitrary = K' <$> arbitrary

instance Arbitrary b => Arbitrary (Flip K' a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip (K' b)

type FlipK'Id = Flip K' (Maybe Char) Integer -> Bool
type FlipK'Comp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> Flip K' (Maybe Char) Integer 
  -> Bool


-- EvilGoateeConst a b
data EvilGoateeConst a b = 
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

type EvilGoateeConstId = EvilGoateeConst Integer (Maybe Char) -> Bool
type EvilGoateeConstComp =
     Fun Integer String 
  -> Fun String [Bool] 
  -> EvilGoateeConst (Maybe Char) Integer
  -> Bool


-- LiftItOut f a
data LiftItOut f a = 
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

instance Arbitrary (f a) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary

type LiftItOutId = LiftItOut Maybe Char -> Bool
type LiftItOutComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -> LiftItOut Maybe Integer
  -> Bool


-- Parappa f g a
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance ( Arbitrary (f a), Arbitrary (g a) ) 
        => Arbitrary (Parappa f g a) where
  arbitrary = DaWrappa <$> arbitrary <*> arbitrary

type ParappaId = Parappa [] Maybe Char -> Bool
type ParappaComp = 
     Fun Integer String 
  -> Fun String [Bool] 
  -- -> Parappa [] Maybe Integer
  -> Parappa (Either Char) Maybe Integer
  -> Bool


-- Main
main :: IO ()
main = do
  quickCheck (functorIdentity :: QuantId)
  quickCheck (functorCompose' :: QuantComp)

  quickCheck (functorIdentity :: KId)
  quickCheck (functorCompose' :: KComp)

  quickCheck (functorIdentity :: FlipK'Id)
  quickCheck (functorCompose' :: FlipK'Comp)

  quickCheck (functorIdentity :: EvilGoateeConstId)
  quickCheck (functorCompose' :: EvilGoateeConstComp)

  quickCheck (functorIdentity :: LiftItOutId)
  quickCheck (functorCompose' :: LiftItOutComp)

  quickCheck (functorIdentity :: ParappaId)
  -- Why does functorCompose' take a long time with type:
  -- ... -> Parappa [] Maybe Integer -> Bool ?
  quickCheck (functorCompose' :: ParappaComp) 
