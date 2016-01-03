{-# LANGUAGE RecordWildCards #-}
module Network.Raptr.RaptrSpec where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.Queue
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans      (liftIO)
import           Data.Binary
import           Data.ByteString.Char8    (unpack)
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
  log <- createLog $ unpack nodeid <.> "log"
  node <- newNode (Just q) raftConfig  (Client raptrNodes) log
  nodethread <- async $ evalStateT (runServer (run raftConfig initialState)) node
  start (r { nodeThread = Just nodethread }) app

raptrSpec = do

  it "runs a 3 node cluster" $ do
    cluster <- localCluster 3

    servers <- forM cluster startServer
    threadDelay $ 10 * 1000 * 1000
    False `shouldBe` True



