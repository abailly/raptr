{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Network.Raptr.Server where

import           Control.Concurrent.MVar
import           Control.Concurrent.Queue as Q
import           Control.Concurrent.STM
import           Data.Binary              (Binary, decode, encode)
import           Data.List                (isPrefixOf, (!!))
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Kontiki.Raft
import           Network.Raptr.Node
import           Network.Raptr.Types
import           Network.Wai


deriving instance Generic (Event a)
instance Binary (Event Value)

server :: MVar (Queue (Event Value)) -> Application
server qVar req sendResponse = do
  withMVar qVar $ \ q -> do
    if [ "raptr" ] `isPrefixOf` pathInfo req  &&
       requestMethod req == "POST"
      then enqueueEvent q req sendResponse
      else sendResponse $ responseBuilder status400 [("Content-Type", "text/plain")] ""

enqueueEvent :: Queue (Event Value) -> Application
enqueueEvent q req sendResponse = do
  let nodeid = encodeUtf8 $ pathInfo req !! 1
  msg <- decode <$> lazyRequestBody req
  let event = EMessage nodeid msg
  isQueued <- atomically $ Q.put q  event
  if isQueued
    then sendResponse $ responseLBS status200 [("Content-Type", "application/octet-stream")] (encode event)
    else sendResponse $ responseLBS status503 [("Content-Type", "text/plain")] "cannot enqueue event because queue is full, try again later"
