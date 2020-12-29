module Ch17_08_Variation_on_Either where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' x) = Success' (f x)

instance Monoid e 
      => Applicative (Validation e) where
  pure = Success'
  (Failure' e) <*> (Failure' e') = Failure' (e <> e')
  (Failure' e) <*> _             = Failure' e
  _            <*> (Failure' e') = Failure' e'
  (Success' f) <*> (Success' v)  = Success' (f v)

instance (Arbitrary e, Arbitrary a)
  => Arbitrary (Validation e a) where
  arbitrary = 
    frequency [ (1, Failure' <$> arbitrary)
              , (3, Success' <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

---------------------
main :: IO ()
main = do
  quickBatch $ functor (undefined :: Validation Char (String, String, Integer))
  quickBatch $ applicative (undefined :: Validation [String] (String, String, Integer))