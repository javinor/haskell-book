module Ch18_07_Exercises where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type SSI = (String, String, Integer)

-- Nope a
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

type NopeQB =
  Nope SSI

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg  


-- BahEither b a
data BahEither b a = 
    PLeft a
  | PRight b
  deriving (Eq, Show)

type BahEitherQB =
  BahEither [Integer] SSI

instance ( Arbitrary a, Arbitrary b ) 
        => Arbitrary (BahEither b a) where
  arbitrary = 
    oneof [ PLeft <$> arbitrary
          , PRight <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance Functor (BahEither b) where
  fmap _ (PRight x) = PRight x
  fmap f (PLeft x) = PLeft $ f x

instance Applicative (BahEither b) where
  pure = PLeft
  (PRight x) <*> _          = PRight x
  _          <*> (PRight y) = PRight y
  (PLeft f)  <*> (PLeft v)  = PLeft $ f v

instance Monad (BahEither b) where
  PRight x >>= _ = PRight x
  PLeft x >>= f = f x


-- Identity a
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

type IdentityQB = Identity SSI

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity v) = Identity $ f v

instance Monad Identity where
  (Identity x) >>= f = f x


-- List a
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = 
  Cons x (append xs ys)

fold :: (a -> b -> b) 
      -> b 
      -> List a 
      -> b
fold _ b Nil = b
fold f b (Cons x xs) =
  f x $ fold f b xs

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f = fold (append . f) Nil

type ListQB = List SSI

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [ (1, return $ Nil)
              , (3, Cons <$> arbitrary <*> arbitrary) ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  -- fmap _ Nil = Nil
  -- fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap f = fold (Cons . f) Nil

instance Applicative List where
  pure x = Cons x Nil

  -- === take 1 ===
  -- Nil <*> _ = Nil
  -- Cons f fs <*> vs =
  --   (f <$> vs) `append` (fs <*> vs)
  
  -- === take 2 ===
  -- fs <*> xs =
  --   fold (\f -> append (fmap f xs)) Nil fs

  -- === take 3 ===
  fs <*> xs =
    flatMap (\f -> f <$> xs) fs


instance Monad List where
  -- Nil >>= _ = Nil
  -- (Cons x xs) >>= f =
  --   f x `append` (xs >>= f)
  (>>=) = flip flatMap


-- Q1
j :: Monad m => m (m a) -> m a
j = join

-- Q2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- Q3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- Q4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- Q5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  y <- f x
  ys <- meh xs f
  return $ y : ys

-- Q6
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id


----- Main -----
main :: IO ()
main = do
  putStrLn "\n\n=== Nope a ==="
  quickBatch $ functor (undefined :: NopeQB)
  quickBatch $ applicative (undefined :: NopeQB)
  quickBatch $ monad (undefined :: NopeQB)

  putStrLn "\n\n=== BahEither b a ==="
  quickBatch $ functor (undefined :: BahEitherQB)
  quickBatch $ applicative (undefined :: BahEitherQB)
  quickBatch $ monad (undefined :: BahEitherQB)

  putStrLn "\n\n=== Identity a ==="
  quickBatch $ functor (undefined :: IdentityQB)
  quickBatch $ applicative (undefined :: IdentityQB)
  quickBatch $ monad (undefined :: IdentityQB)

  putStrLn "\n\n=== List a ==="
  quickBatch $ functor (undefined :: ListQB)
  quickBatch $ applicative (undefined :: ListQB)
  quickBatch $ monad (undefined :: ListQB)

  putStrLn "\n\n=== Questions (should all be True) ==="
  print $ j [[1, 2], [], [3]] == [1,2,3]
  print $ j (Just (Just 1)) == Just 1
  print $ j (Just Nothing) == (Nothing :: Maybe Integer)
  print $ j Nothing == (Nothing :: Maybe Integer)



