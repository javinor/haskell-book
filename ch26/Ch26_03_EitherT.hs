{-# LANGUAGE InstanceSigs #-}

module Ch26_03_EitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m) 
      => Functor (EitherT e m) where
  fmap f (EitherT m) = 
    EitherT $ (fmap . fmap) f m

instance (Applicative m) 
      => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  EitherT f <*> EitherT a = 
    EitherT $ (<*>) <$> f <*> a

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  
  EitherT m >>= k = EitherT $ do
    x <- m
    case x of
      Left e -> return $ Left e
      Right a -> (runEitherT . k) a


-- 
swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT m) = EitherT $ fmap swapEither m

-- 
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT m) = m >>= either f g