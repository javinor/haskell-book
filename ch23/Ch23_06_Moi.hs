{-# LANGUAGE InstanceSigs #-}

module Ch23_06_Moi where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Show (Moi s a) where
  show _ = "Moi s a"

instance ( CoArbitrary s
         , Arbitrary a
         , Arbitrary s
      ) => Arbitrary (Moi s a) where
  arbitrary = Moi <$> arbitrary

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = 
    Moi $ \s -> 
            let (a, s') = g s
            in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  
  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = 
    Moi $ \s -> 
            let (ab, s') = f s
                (a, s'') = g s'
            in (ab a, s'')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g =
    Moi $ \s -> let (a, s') = f s
                    (b, s'') = runMoi (g a) s'
                in (b, s'')

-- ===== QuickCheck Laws =====
-- Functor
prop_functorId 
  :: (Eq a, Eq s) 
  => Moi s a -> s -> Bool
prop_functorId m s =
  runMoi (fmap id m) s == runMoi m s

prop_functorComp 
  :: (Eq c, Eq s) 
  => Moi s a -> Fun a b -> Fun b c -> s -> Bool
prop_functorComp m f g s = 
  runMoi (fmap (g' . f') m) s == runMoi (fmap g' . fmap f' $ m) s
  where f' = applyFun f
        g' = applyFun g

-- Applicative
prop_applicativeId :: (Eq s, Eq a) => Moi s a -> s -> Bool
prop_applicativeId m s =
  runMoi (pure id <*> m) s == runMoi m s

prop_applicativeHomomorphism 
  :: (Eq s, Eq b) 
  => Fun a b -> a -> s -> Bool
prop_applicativeHomomorphism f a s =
  runMoi (pure f' <*> pure a) s == runMoi (pure (f' a)) s
  where f' = applyFun f

prop_applicativeInterchange 
  :: (Eq b, Eq s) 
  => Fun a b -> a -> s -> Bool
prop_applicativeInterchange f a s = 
  runMoi (pure f' <*> pure a) s == runMoi (pure ($ a) <*> pure f') s
  where f' = applyFun f

prop_applicativeComp :: (Eq c, Eq s) => Fun b c -> Fun a b -> Moi s a -> s -> Bool
prop_applicativeComp f g m s =
  runMoi (pure (.) <*> pure f' <*> pure g' <*> m) s == runMoi (pure f' <*> (pure g' <*> m)) s
  where f' = applyFun f
        g' = applyFun g

-- Monad
prop_monadLeftId :: (Eq b, Eq s) => Fun a (Moi s b) -> a -> s -> Bool
prop_monadLeftId f a s =
  runMoi (return a >>= f') s == runMoi (f' a) s
  where f' = applyFun f

prop_monadRightId :: (Eq a, Eq s) => Moi s a -> s -> Bool
prop_monadRightId m s = 
  runMoi (m >>= return) s == runMoi m s

prop_monadAssoc :: (Eq c, Eq s) => Fun a (Moi s b) -> Fun b (Moi s c) -> Moi s a -> s -> Bool
prop_monadAssoc f g m s =
  runMoi (m >>= f' >>= g') s == runMoi (m >>= (\x -> f' x >>= g')) s
  where f' = applyFun f
        g' = applyFun g


main :: IO ()
main = do
  let f = (+1) <$> (Moi $ \s -> (0, s))
  print $ runMoi f 0

  quickCheck (prop_functorId :: Moi Int String -> Int -> Bool)
  quickCheck (prop_functorComp 
                :: Moi Int String -> Fun String Int -> Fun Int Char -> Int -> Bool)
  
  quickCheck (prop_applicativeId :: Moi Int String -> Int -> Bool)
  quickCheck (prop_applicativeHomomorphism 
                :: Fun Char String -> Char -> Int -> Bool)
  quickCheck (prop_applicativeInterchange 
                :: Fun Char String -> Char -> Int -> Bool)
  quickCheck (prop_applicativeComp 
                :: Fun String Integer -> Fun Char String -> Moi Int Char -> Int -> Bool)

  quickCheck (prop_monadLeftId 
                :: Fun String (Moi Int Char) -> String -> Int -> Bool)
  quickCheck (prop_monadRightId 
                :: Moi Int Char -> Int -> Bool)
  quickCheck (prop_monadAssoc
                :: Fun Char (Moi Int String) 
                -> Fun String (Moi Int Integer) 
                -> Moi Int Char 
                -> Int 
                -> Bool)
