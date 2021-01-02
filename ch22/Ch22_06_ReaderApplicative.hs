{-# LANGUAGE InstanceSigs #-}

module Ch22_06_ReaderApplicative where

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap fab (Reader ra) = 
    -- Reader $ \r -> fab (ra r)
    Reader $ fab . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)