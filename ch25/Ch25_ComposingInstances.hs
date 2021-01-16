{-# LANGUAGE InstanceSigs #-}

module Ch25_ComposingInstances where

newtype Identity a =
  Identity { runIdentity :: a }

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

-- Functor
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

-- Applicative
instance (Applicative f, Applicative g)
      => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ fmap (<*>) f <*> a

-- Foldable
instance (Foldable f, Foldable g) 
      => Foldable (Compose f g) where
  foldMap :: (Foldable f, Foldable g, Monoid m) 
          => (a -> m) -> Compose f g a -> m
  -- foldMap h (Compose fga) = foldMap (foldMap h) fga
  foldMap f = foldMap (foldMap f) . getCompose

-- Traversable
instance (Traversable t, Traversable t1) 
      => Traversable (Compose t t1) where
  traverse :: Applicative f => (a -> f b) -> Compose t t1 a -> f (Compose t t1 b)
  traverse h (Compose fga) = 
    Compose <$> traverse (traverse h) fga