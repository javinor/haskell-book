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
  -> Parappa (Either Char) Maybe Integer
  -> Bool


-- IgnoreOne f g a b
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = 
    IgnoringSomething fa (fmap f gb) 

instance (Arbitrary (f a),
          Arbitrary (g b)) 
       => Arbitrary (IgnoreOne f g a b) where
  arbitrary = IgnoringSomething 
          <$> arbitrary 
          <*> arbitrary

type IgnoreOneId = IgnoreOne [] Maybe Bool Integer -> Bool
type IgnoreOneComp =
     Fun Integer String 
  -> Fun String [Bool] 
  -> IgnoreOne (Either Char) Maybe Bool Integer
  -> Bool


-- Notorious g o a t
data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance ( Arbitrary (g o)
         , Arbitrary (g a)
         , Arbitrary (g t)
         ) => Arbitrary (Notorious g o a t) where
  arbitrary = Notorious
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary

type NotoriousId = Notorious Maybe Bool Char Integer -> Bool
type NotoriousComp =
     Fun Integer String 
  -> Fun String [Bool] 
  -> Notorious Maybe Bool Char Integer 
  -> Bool


-- List a
data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = 
    frequency [ (1, return $ Nil)
              , (3, Cons <$> arbitrary <*> arbitrary)]

type ListId = List Integer -> Bool
type ListComp =
     Fun Integer String 
  -> Fun String [Bool] 
  -> List Integer
  -> Bool


-- GoatLord a
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x) 
  fmap f (MoreGoats gla glb glc) = 
    MoreGoats (fmap f gla)
              (fmap f glb)
              (fmap f glc)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = 
    frequency [ (10, return NoGoat)
              , (10, OneGoat <$> arbitrary)
              , (7, MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary) ]

type GoatLordId = GoatLord Integer -> Bool
type GoatLordComp =
     Fun Integer String 
  -> Fun String [Bool] 
  -> GoatLord Integer
  -> Bool


-- TalkToMe a
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Show a => Show (TalkToMe a) where 
  show Halt = "Halt"
  show (Print s a) = "Print " ++ s ++ " " ++ show a
  show (Read _) = "Read _"

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read fsa)  = Read $ fmap f fsa

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary =
    oneof [ return Halt
          , Print <$> arbitrary <*> arbitrary
          , Read <$> arbitrary
          ]

type TalkToMeId = TalkToMe Integer -> String -> Bool
type TalkToMeComp =
     Fun Integer String 
  -> Fun String [Bool] 
  -> TalkToMe Integer
  -> String
  -> Bool

functorIdTTM :: Eq a => TalkToMe a -> String -> Bool
functorIdTTM ttm rndStr =
  let ttm' = fmap id ttm
  in case (ttm, ttm') of
    (Halt, Halt)           -> True
    (Print s a, Print t b) -> s == t && a == b
    (Read f, Read g)       -> f rndStr == g rndStr
    _                      -> False

functorComposeTTM :: Eq c 
  => Fun a b -> Fun b c -> TalkToMe a -> String -> Bool
functorComposeTTM fab gbc ttm rndStr =
  let f = applyFun fab
      g = applyFun gbc
      left = fmap g (fmap f ttm)
      right = fmap (g . f) ttm
  in case (left, right) of
    (Halt, Halt)           -> True
    (Print s a, Print t b) -> s == t && a == b
    (Read fn, Read fn')       -> fn rndStr == fn' rndStr
    _ -> False


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
  quickCheck (functorCompose' :: ParappaComp) 

  quickCheck (functorIdentity :: IgnoreOneId)
  quickCheck (functorCompose' :: IgnoreOneComp) 

  quickCheck (functorIdentity :: NotoriousId)
  quickCheck (functorCompose' :: NotoriousComp) 

  quickCheck (functorIdentity :: ListId)
  quickCheck (functorCompose' :: ListComp) 

  quickCheck (functorIdentity :: GoatLordId)
  quickCheck (functorCompose' :: GoatLordComp) 

  quickCheck (functorIdTTM :: TalkToMeId)
  quickCheck (functorComposeTTM :: TalkToMeComp) 
