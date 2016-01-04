{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.Raptr.RaptrSpec where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.Queue
import           Control.Exception
import           Control.Lens             ((^.))
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans      (liftIO)
import           Data.Binary
import           Data.ByteString.Char8    (unpack)
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.Raptr.Raptr
import           Network.URI
import           Network.Wai              (Application)
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Hspec

startServer :: Raptr -> IO Raptr
startServer r@Raptr{..} = do
  putStrLn $ "starting raptr " ++ show r
  q <- newQueueIO 10
  m <- newMVar q
  let app = server m
      nodeid = _configNodeId raftConfig
  log <- openLog $ unpack nodeid <.> "log"
  node <- newNode (Just q) raftConfig  (Client raptrNodes) log
  nodethread <- async $ runReaderT (runServer (run raftConfig initialState)) node
  start (r { nodeThread = Just nodethread }) app

raptrSpec = do

  it "runs a 3 node cluster" $ do
    servers <- localCluster 3 >>= mapM startServer

    manager <- newManager defaultManagerSettings
    request <- parseUrl "http://localhost:30700/raptr/"
    let req = request { checkStatus = \ s h ck -> Nothing
                      , method = "PUT"
                      , requestBody = RequestBodyLBS "1234567890" }
    response <- httpLbs req manager

    forM_ servers stop

    statusCode (responseStatus response)  `shouldBe` 201



