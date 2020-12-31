module Ch20_04_FoldableInstances where

import Data.Monoid

-- Identity a
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- Optional a
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr f z Nada    = z
  foldr f z (Yep x) = f x z

  foldl f z Nada    = z
  foldl f z (Yep x) = f z x

  foldMap f Nada    = mempty
  foldMap f (Yep x) = f x
