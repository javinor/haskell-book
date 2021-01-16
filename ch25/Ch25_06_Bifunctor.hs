module Ch25_06_Bifunctor where

class Bifunctor p where
  -- {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g
  
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- Deux a b
data Deux a b = 
  Deux a b 
  deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b) = Deux (f a) b
  second g (Deux a b) = Deux a (g b)


-- Const a b
data Const a b = 
  Const a
  deriving (Eq, Show)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  first f (Const a) = Const (f a)
  second _ (Const a) = Const a


-- Drei a b c
data Drei a b c = 
  Drei a b c
  deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c) = Drei a (f b) c
  second g (Drei a b c) = Drei a b (g c)


-- SuperDrei a b c
data SuperDrei a b c = 
  SuperDrei a b
  deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)
  first f (SuperDrei a b) = SuperDrei a (f b)
  second _ (SuperDrei a b) = SuperDrei a b


-- SemiDrei a b c
data SemiDrei a b c = 
  SemiDrei a
  deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a
  first _ (SemiDrei a) = SemiDrei a
  second _ (SemiDrei a) = SemiDrei a


-- Quadriceps a b c d =
data Quadriceps a b c d =
  Quadzzz a b c d
  deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)
  first f (Quadzzz a b c d) = Quadzzz a b (f c) d
  second g (Quadzzz a b c d) = Quadzzz a b c (g d)

-- Either a b 
data Either a b =
    Left a
  | Right b

instance Bifunctor Either where
  bimap f g (Left a) = Left (f a)
  bimap f g (Right b) = Right (g b)

  first f (Left a) = Left (f a)
  first _ (Right b) = Right b

  second _ (Left a) = Left a
  second g (Right b) = Right (g b)