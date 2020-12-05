module Ch06_05_EqInstances where

-- 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  TisAn n == TisAn m = n == m


-- 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two a b == Two i j = a == i && b == j


-- 3
data StringOrInt = 
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt n == TisAnInt m = n == m
  TisAString s == TisAString s' = s == s'
  _ == _ = False


-- 4
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  Pair a b == Pair a' b' = a == a' && b == b'


-- 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'


-- 6
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne a == ThatOne a' = a == a'
  _ == _ = False


-- 7
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x == y
  Goodbye x == Goodbye y = x == y
  _ == _ = False