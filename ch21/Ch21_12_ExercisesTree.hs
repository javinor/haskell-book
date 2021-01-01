module Ch21_12_ExercisesTree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency [ (2, return Empty)
              , (2, Leaf <$> arbitrary)
              , (3, Node <$> arbitrary <*> arbitrary <*> arbitrary) ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node left x right) = 
    Node (fmap f left) (f x) (fmap f right)

instance Foldable Tree where
  -- foldMap :: (Foldable t, Monoid m) 
  --         => (a -> m) -> t a -> m
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node left x right) = 
    foldMap f left <> f x <> foldMap f right
  
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  -- TODO why does this fail some QuickCheck tests?
  -- foldr f z (Node left x right) = 
  --   f x $ foldr f (foldr f z left) right
  foldr f z (Node left x right) = 
    foldr f (f x (foldr f z right)) left

instance Traversable Tree where
  -- traverse :: (Traversable t, Applicative f) 
  --          => (a -> f b) -> t a -> f (t b)
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node left x right) = 
    Node <$> traverse f left
         <*> f x
         <*> traverse f right

main :: IO ()
main = do
  -- sample (arbitrary :: Gen (Tree Int))
  quickBatch $ functor (undefined :: Tree (Int, Int, String))
  quickBatch $ foldable (undefined :: Tree (Int, Int, String, Integer, Char))
  quickBatch $ traversable (undefined :: Tree (Int, Int, String))
