{-# LANGUAGE InstanceSigs #-}

module Ch25_08_IdentityT where

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)


-- Functor
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT (fmap f ma)


-- Applicative
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) =
    Identity (f a)

instance (Applicative m) 
      => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)

  IdentityT mf <*> IdentityT ma = 
    IdentityT (mf <*> ma)


-- Monad
instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  IdentityT ma >>= f =
    IdentityT $ ma >>= runIdentityT . f
