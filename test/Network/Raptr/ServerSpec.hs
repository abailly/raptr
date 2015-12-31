{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Raptr.ServerSpec where

import           Control.Concurrent.MVar
import           Control.Concurrent.Queue
import           Control.Exception
import           Data.Binary
import           Network.Raptr.Raptr
import           Network.URI
import           Network.Wai              (Application)
import           Test.Hspec
import           Test.Hspec.Wai

app :: IO Application
app = do
  q <- newQueueIO 10
  mvar <- newMVar q
  return $ server mvar

startStopServer action = bracket
                         (newQueueIO 10 >>= newMVar >>= start defaultConfig . server)
                         stop
                         action
clientSpec :: Spec
clientSpec = around startStopServer $ do

  it "can send message from client to server" $ \ srv -> do
    let p = raptrPort srv
        Just uri = parseURI $ "http://localhost:" ++ show p ++"/raptr/bar"
        client = NodeClient "foo" uri
        msg :: Message Value = MRequestVote $ RequestVote term0 "foo" index0 term0
    sendClient msg client -- expect no exception

serverSpec :: Spec
serverSpec = with app $ do

  it "on POST /raptr/foo it enqueues event and returns it" $ do
    let msg :: Message Value = MRequestVote $ RequestVote term0 "foo" index0 term0
        ev = EMessage "foo" msg
    post "/raptr/foo" (encode msg) `shouldRespondWith` ResponseMatcher { matchStatus = 200
                                                                       , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
                                                                       , matchBody = Just $ encode ev }

