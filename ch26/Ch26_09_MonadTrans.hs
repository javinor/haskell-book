module Ch26_09_MonadTrans where

import Ch26_02_MaybeT
import Ch26_03_EitherT
import Ch26_04_ReaderT
import Ch26_05_StateT
import Control.Monad (liftM)
import Control.Monad.Trans.Class

-- class MonadTrans t where
-- -- | Lift a computation from
-- -- the argument monad to
-- -- the constructed monad.
--   lift :: (Monad m) => m a -> t m a

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift ma = 
    StateT $ \s -> do
      a <- ma
      return (a, s)