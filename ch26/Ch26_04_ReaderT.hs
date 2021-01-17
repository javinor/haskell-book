{-# LANGUAGE InstanceSigs #-}

module Ch26_04_ReaderT where

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) 
      => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = 
    -- ReaderT $ (fmap . fmap) rma
    ReaderT $ (fmap f) . rma

instance (Applicative m)
      => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  ReaderT f <*> ReaderT a =
    ReaderT $ (<*>) <$> f <*> a

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  ReaderT rma >>= k = ReaderT $ \r -> do
    a <- rma r
    runReaderT (k a) r