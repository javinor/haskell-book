module Ch26_08_OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = return 1

maybeUnwrap :: ExceptT String
               (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- Next
eitherUnwrap :: ReaderT () IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- Lastly
readerUnwrap :: ()
             -> IO (Either String
                           (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- main :: IO ()
-- main =  readerUnwrap ()
-- expected result: Right (Just 1)

-- Exercise: Wrap up it
embedded' :: MaybeT
             (ExceptT String
                      (ReaderT () IO))
             Int
embedded' = MaybeT . ExceptT . ReaderT $ return . (const (Right (Just 1)))

