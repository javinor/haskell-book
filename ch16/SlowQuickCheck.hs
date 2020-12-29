-- Asked on SO:
-- https://stackoverflow.com/questions/65491718/why-does-quickcheck-take-a-long-time-when-testing-a-functor-instance-with-a-spec

module SlowQuickCheck where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) 
               => Fun a b -> Fun b c -> f a -> Bool
functorCompose' fab gbc x =
  (fmap g (fmap f x)) == (fmap (g . f) x)
  where f = applyFun fab
        g = applyFun gbc


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


-- Main
main :: IO ()
main = do
  quickCheck (functorIdentity :: ParappaId)
  quickCheck (functorCompose' :: ParappaComp) 
