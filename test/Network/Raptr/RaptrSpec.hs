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
import           Data.List                (nub)
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.Raptr.Raptr
import           Network.Raptr.TestUtils
import           Network.URI
import           Network.Wai              (Application)
import           System.FilePath
import           System.IO                (IOMode (..), hFileSize,
                                           withBinaryFile)
import           System.IO.Storage        (getAllData)
import           Test.Tasty
import           Test.Tasty.Hspec

logs = [ "node1.log", "node2.log", "node3.log" ]

cleanUpLogs :: IO ()
cleanUpLogs = removeFiles logs

tryInsertingEntry _       _   _     0     = return Nothing
tryInsertingEntry manager url datum count = do
  request <- parseUrl url
  let req = request { method = "PUT"
                    , requestBody = RequestBodyLBS datum
                    , redirectCount = 0 }
  (Just <$> httpLbs req manager) `catch` \(StatusCodeException s h _) ->
                                          case statusCode s of
                                           302 -> case lookup hLocation h of
                                                   Nothing -> return Nothing
                                                   Just u  -> tryInsertingEntry manager (unpack u) datum count
                                           503 -> threadDelay (1000 * 1000) >> tryInsertingEntry manager url datum (count - 1)
                                           _   -> return Nothing


raptrSpec = before_ cleanUpLogs $ do

  it "runs a 3 node cluster" $ do
    servers <- localCluster 3 >>= mapM startServer

    threadDelay $ 10 * 1000 * 1000 -- wait for election of a leader

    manager <- newManager defaultManagerSettings
    response <- tryInsertingEntry manager "http://localhost:30700/raptr/" "1234567890" 3

    when (response /= Nothing) $ threadDelay $ 5 * 1000 * 1000 -- wait for propagation to all logs

    forM_ servers stop

    (statusCode . responseStatus) <$> response  `shouldBe` Just 201

    forM_ logs (\ l -> do
                   flog <- openLog l
                   datum <- getAllData flog
                   putStrLn $ "got data " ++ show datum
                   datum `shouldBe` [ "1234567890" ]
               )



