module Ch21_12_Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type ICS = (Integer, Char, String)

-- Identity a
newtype Identity a = 
  Identity a
  deriving (Eq, Ord, Show)

type IdentityQB = Identity ICS

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
  traverse f (Identity x) = Identity <$> f x


-- Constant a b
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

type ConstantQB =
  Constant Int ICS

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a


-- Optional a
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

type OptionalQB = Optional ICS

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [ (1, return Nada)
              , (3, Yep <$> arbitrary) ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x


-- List a
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

type ListQB = List ICS

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [ (1, return $ Nil)
              , (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs


-- Three a b c
data Three a b c =
  Three a b c
  deriving (Eq, Show)

type ThreeQB = Three Bool Char ICS

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         ) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z


-- Pair a b
data Pair a b =
  Pair a b
  deriving (Eq, Show)

type PairQB = Pair Bool ICS

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair x y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = Pair x <$> f y


-- Big a b
data Big a b =
  Big a b b
  deriving (Eq, Show)

type BigQB = Big Bool ICS

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary
                  <*> arbitrary
                  <*> arbitrary
      
instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance Functor (Big a) where
  fmap f (Big x y y') = Big x (f y) (f y')

instance Foldable (Big a) where
  foldMap f (Big _ y y') = f y <> f y' -- ?

instance Traversable (Big a) where
  traverse f (Big x y y') = Big x <$> f y <*> f y'


-- Bigger a b
data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

type BiggerQB = Bigger Bool ICS

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq
 
instance Functor (Bigger a) where
  fmap f (Bigger a x y z) = Bigger a (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ x y z) = f x <> f y <> f z

instance Traversable (Bigger a) where
  traverse f (Bigger a x y z) = 
    Bigger a <$> f x <*> f y <*> f z


-------------
main :: IO ()
main = do
  putStrLn "\n\n=== Identity a ==="
  quickBatch (traversable (undefined :: IdentityQB))
  
  putStrLn "\n\n=== Constant a b ==="
  quickBatch (traversable (undefined :: ConstantQB))

  putStrLn "\n\n=== Optional a ==="
  quickBatch (traversable (undefined :: OptionalQB))

  putStrLn "\n\n=== List a ==="
  quickBatch (traversable (undefined :: ListQB))

  putStrLn "\n\n=== Three a b c ==="
  quickBatch (traversable (undefined :: ThreeQB))

  putStrLn "\n\n=== Pair a b ==="
  quickBatch (traversable (undefined :: PairQB))

  putStrLn "\n\n=== Big a b ==="
  quickBatch (traversable (undefined :: BigQB))

  putStrLn "\n\n=== Bigger a b ==="
  quickBatch (traversable (undefined :: BiggerQB))