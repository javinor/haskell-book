module Ch17_05_Exercise_Instances where


-- Identity a
newtype Identity a = 
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)


-- Constant a b
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f ca = Constant $ getConstant ca

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  ca <*> cb = 
    let a = getConstant ca
        b = getConstant cb
    in Constant $ a <> b