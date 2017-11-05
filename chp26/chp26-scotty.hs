{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Tuple
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config {
  counts :: IORef (M.Map Text Integer),
  prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = ((+1) . fromMaybe 0) <$> swap (M.insertLookupWithKey add1 k 1 m)
  where add1 _ _ v = v + 1

app :: Scotty ()
app = 
  get "/:key" $ do
     unprefixed <- param "key"
     (prefix, ioCounts) <- lift $ (,) <$> prefix <*> counts <$> ask
     counts <- liftIO $ readIORef ioCounts
     let key = mappend prefix unprefixed
     let (newCounts, newInteger) = bumpBoomp key counts
     liftIO $ writeIORef ioCounts newCounts
     html $ mconcat [ "<h1> Success! Count was: ", TL.pack $ show newInteger, "<h1>"]

main :: IO ()
main = do
[prefixArg] <- (fmap . fmap) TL.pack getArgs
counter <- newIORef M.empty
let config = Config counter prefixArg
    runR r = runReaderT r config
scottyT 3000 runR app
