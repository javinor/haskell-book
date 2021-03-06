{-# LANGUAGE OverloadedStrings #-}

module Ch26_14_Exercise_HitCounter where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (pack, Text, unpack)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
  -- that's one, one click!
  -- two... two clicks!
  -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = 
  let newVal = case M.lookup k m of
                 Nothing -> 1
                 Just n -> n + 1
      newMap = M.insert k newVal m
  in (newMap, newVal)

app :: Scotty ()
app =
  get "/:key" $ do
    config <- lift ask
    unprefixed <- param "key"
    let key' = mappend (prefix config) unprefixed
    newInteger <- liftIO $ atomicModifyIORef' (counts config) (bumpBoomp key')
    
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app