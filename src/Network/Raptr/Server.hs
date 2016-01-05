{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Network.Raptr.Server where

import           Control.Concurrent.MVar
import           Control.Concurrent.Queue   as Q
import           Control.Concurrent.STM
import           Control.Monad.Reader       (runReaderT)
import           Data.Binary                (Binary, decode, encode)
import qualified Data.ByteString.Char8      as BS8
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.List                  (isPrefixOf, (!!))
import           Data.Monoid                ((<>))
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Kontiki.Raft
import           Network.Raptr.Node
import           Network.Raptr.Types
import           Network.Wai


deriving instance Generic (Event a)
instance Binary (Event Value)

trace node s = putStrLn $ "[" <> BS8.unpack (nodeId node) <> "] - " <> s

server :: Node -> Application
server node req sendResponse = do
  trace node $ "server got request: " ++ show req
  if [ "raptr" ] `isPrefixOf` pathInfo req
    then case requestMethod req of
          "POST" -> enqueueEvent node req sendResponse
          "PUT"  -> addNewEntry node req sendResponse
          _      -> sendError
    else sendError
  where
    sendError = sendResponse $ responseBuilder status400 [("Content-Type", "text/plain")] ""

addNewEntry :: Node -> Application
addNewEntry node req sendResponse = do
  datum <- requestBody req
  result <- runReaderT (runServer $ storeEntry datum) node
  case result of
   StoredEntry e -> trace node ("server stored new entry " ++ show e) >>
                    sendResponse (responseLBS status201 [("Content-Type", "application/octet-stream")] (encode e))
   TryAgainLater -> trace node ("server cannot handle adding new entry now as there are no leader, try again later") >>
                    sendResponse (responseLBS status503 [("Content-Type", "text/plain")] (pack $ "cannot store new entry, try again later"))
   NotLeader nid uri -> trace node ("redirecting to leader node " ++ show nid ++ " at URI " ++ show uri) >>
                        sendResponse (responseLBS status302 [("Content-Type", "text/plain"), ("Location", BS8.pack $ show uri)] "")
   ErrorStoringEntry e -> trace node ("got error while trying to store entry: " ++ show e) >>
                          sendResponse (responseLBS status500 [("Content-Type", "text/plain")] (pack $ "cannot store new entry, got error : " ++ e))

  where

enqueueEvent :: Node -> Application
enqueueEvent node@Node{..} req sendResponse = do
  let nodeid = encodeUtf8 $ pathInfo req !! 1
  msg <- decode <$> lazyRequestBody req
  let event = EMessage nodeid msg
  isQueued <- atomically $ Q.put nodeInput event

  if isQueued
    then trace node ("server enqueued event " ++ show event) >> sendResponse (responseLBS status200 [("Content-Type", "application/octet-stream")] (encode event))
    else sendResponse $ responseLBS status503 [("Content-Type", "text/plain")] "cannot enqueue event because queue is full, try again later"
