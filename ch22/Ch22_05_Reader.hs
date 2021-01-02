module Ch22_05_Reader where

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap fab (Reader ra) = 
    -- Reader $ \r -> fab (ra r)
    Reader $ fab . ra

ask :: Reader a a
ask = Reader id