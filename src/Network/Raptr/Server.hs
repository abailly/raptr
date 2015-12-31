{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.Raptr.Server where

import           Control.Concurrent.MVar
import           Control.Concurrent.Queue as Q
import           Control.Concurrent.STM
import           Data.Binary              (decode)
import           Data.List                (isPrefixOf, (!!))
import           Data.Text.Encoding
import           Network.HTTP.Types
import           Network.Kontiki.Raft
import           Network.Raptr.Node
import           Network.Raptr.Types
import           Network.Wai


raptrServer :: MVar (Queue (Event Value)) -> Application
raptrServer qVar req sendResponse = do
  withMVar qVar $ \ q -> do
    if [ "raptr" ] `isPrefixOf` pathInfo req  &&
       requestMethod req == "POST"
      then do
      let nodeid = encodeUtf8 $ pathInfo req !! 1
      msg <- decode <$> lazyRequestBody req
      atomically $ Q.put q (EMessage nodeid msg)
      sendResponse $ responseBuilder status200 [("Content-Type", "text/plain")] ""
      else sendResponse $ responseBuilder status400 [("Content-Type", "text/plain")] ""


