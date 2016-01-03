{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | HTTP Client to communicate with a Raptr server
module Network.Raptr.Client where

import           Control.Monad        (forM_)
import qualified Data.Binary          as B
import           Data.Binary.Put      (runPut)
import qualified Data.ByteString      as BS
import           Data.Map             as Map
import qualified Data.Text.Encoding   as E
import           Network.HTTP.Client
import           Network.Kontiki.Raft
import           Network.Raptr.Types
import           Network.URI

data NodeClient = NodeClient { clientNodeId   :: NodeId
                             , clientEndpoint :: URI
                             }

type RaptrNodes = Map.Map NodeId URI

emptyNodes :: RaptrNodes
emptyNodes = Map.empty

data Client a = Client { nodes :: Map.Map NodeId NodeClient }

doBroadcast :: Client Value -> Message Value -> IO ()
doBroadcast c@Client{..} message =
  forM_ (Map.elems nodes) (sendClient message)

doSend :: Client Value -> NodeId ->  Message Value -> IO ()
doSend Client{..} node message =
  case Map.lookup node nodes of
   Nothing     -> return ()  -- TODO warning? cleanup com?
   Just client  -> sendClient message client

sendClient :: Message Value -> NodeClient -> IO ()
sendClient message NodeClient{..} =  do
  manager <- newManager defaultManagerSettings

  request <- parseUrl (uriToString id clientEndpoint $ "")
  let req = request { checkStatus = \ s h ck -> Nothing
                    , method = "POST"
                    , requestBody = RequestBodyLBS $ runPut $ B.put message }

  response <- httpLbs req manager
  return ()
