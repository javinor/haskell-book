{-# LANGUAGE InstanceSigs #-}

module Ch26_05_StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sm) = 
    StateT $ \s -> 
      let f' = \(a,s) -> (f a, s)
      in fmap f' (sm s)
              
instance Monad m 
      => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT smfs <*> StateT smas = 
    StateT $ \s -> do 
      (f, s') <- smfs s
      (a, s'') <- smas s'
      return (f a, s'')
      
instance Monad m
      => Monad (StateT s m) where
  return = pure
  sma >>= f = 
    StateT $ \s -> do
      (a, s') <- runStateT sma s
      (runStateT . f) a s'