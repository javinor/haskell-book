{-# LANGUAGE InstanceSigs #-}

module Ch26_02_MaybeT where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
      => Functor (MaybeT m) where
  fmap f (MaybeT m) = 
    MaybeT $ (fmap . fmap) f m

instance (Applicative m) 
      => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  MaybeT f <*> MaybeT a =
    MaybeT $ fmap (<*>) f <*> a

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  m >>= k = MaybeT $ do
    x <- runMaybeT m
    case x of
      Nothing -> return Nothing
      Just a -> runMaybeT (k a)
