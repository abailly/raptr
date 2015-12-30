{-# LANGUAGE RecordWildCards #-}
-- | HTTP Client to communicate with a Raptr server
module Network.Raptr.Client where

import           Control.Monad        (forM_)
import           Data.Map             as Map
import           Network.HTTP.Client
import           Network.Kontiki.Raft
import           Network.Raptr.Types
import           Network.URI

data NodeClient = NodeClient { clientNodeId   :: NodeId
                             , clientEndpoint :: URI
                             }

data Communicator a = Communicator { nodes :: Map.Map NodeId NodeClient }

doBroadcast :: Communicator Value -> Message Value -> IO ()
doBroadcast c@Communicator{..} message =
  forM_ (Map.elems nodes) (sendClient message)



doSend :: Communicator Value -> NodeId ->  Message Value -> IO ()
doSend Communicator{..} node message =
  case Map.lookup node nodes of
   Nothing     -> return ()  -- TODO warning? cleanup com?
   Just client  -> sendClient message client

sendClient :: Message Value -> NodeClient -> IO ()
sendClient message NodeClient{..} =  do
  manager <- newManager defaultManagerSettings
  request <- parseUrl (uriToString id clientEndpoint $ "")
  response <- httpLbs (request { checkStatus = \ s h ck -> Nothing }) manager
  return ()
