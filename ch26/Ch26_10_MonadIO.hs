module Ch26_10_MonadIO where

import Ch26_02_MaybeT
import Ch26_04_ReaderT
import Ch26_05_StateT
import Ch26_09_MonadTrans
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- class (Monad m) => MonadIO m where
--   -- | Lift a computation
--   -- from the 'IO' monad.
--   liftIO :: IO a -> m a

instance (MonadIO m)
      => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m)
      => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m)
      => MonadIO (StateT s m) where
  liftIO = lift . liftIO