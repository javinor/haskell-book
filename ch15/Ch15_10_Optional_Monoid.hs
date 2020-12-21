module Ch15_10_Optional_Monoid where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (Only x) <> (Only y) = Only (x <> y)
  (Only x) <> _        = Only x
  _        <> (Only y) = Only y
  Nada     <> Nada     = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)


