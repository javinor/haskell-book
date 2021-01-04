{-# LANGUAGE TupleSections #-}

module Ch23_08_Exercises where

import Ch23_06_Moi hiding (main)
import Control.Monad.Trans.State hiding (get, modify, modify', put)

-- Q1 get
get :: State s s
-- get = state $ \x -> (x,x)
-- get = state $ (,) <$> id <*> id
get = state $ (,) <*> id

get' :: Moi s s
get' = Moi $ (,) <*> id


-- Q2 put
put :: s -> State s ()
-- put s = state $ \_ -> ((), s)
put s = state $ const ((), s)

put' :: s -> Moi s ()
put' s = Moi $ const ((), s)


-- Q3 exec
exec :: State s a -> s -> s
-- exec sa s = snd $ runState sa s
exec = (snd .) . runState

exec' :: Moi s a -> s -> s
exec' = (snd .) . runMoi


-- Q4 eval
eval :: State s a -> s -> a
-- eval sa s = fst $ runState sa s
eval = (fst .) . runState

eval' :: Moi s a -> s -> a
eval' = (fst .) . runMoi


-- Q5 modify
modify :: (s -> s) -> State s ()
-- modify f = state $ \s -> ((), f s) 
modify f = state $ ((),) . f 

modify' :: (s -> s) -> Moi s ()
modify' f = Moi $ ((),) . f 


------------------
main :: IO ()
main = do
  print $ runState get "curryIsAmazing"
  print $ runMoi get' "curryIsAmazing"

  print $ runState (put "blah") "woot"
  print $ runMoi (put' "blah") "woot"

  print $ exec (put "wilma") "daphne"
  print $ exec get "scooby papu"
  print $ exec' (put' "wilma") "daphne"
  print $ exec' get' "scooby papu"

  print $ eval get "bunnicula"
  print $ eval get "stake a bunny"
  print $ eval' get' "bunnicula"
  print $ eval' get' "stake a bunny"

  let f = modify (+1)
  print $ runState f 0
  print $ runState (f >> f) 0
  let f' = modify' (+1)
  print $ runMoi f' 0
  print $ runMoi (f' >> f') 0
  print $ runMoi (f' >>= \_ -> f') 0