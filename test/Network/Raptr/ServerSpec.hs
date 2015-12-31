{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Raptr.ServerSpec where

import           Control.Concurrent.MVar
import           Control.Concurrent.Queue
import           Data.Binary
import           Network.Raptr.Raptr
import           Network.Wai              (Application)
import           Test.Hspec
import           Test.Hspec.Wai


app :: IO Application
app = do
  q <- newQueueIO 10
  mvar <- newMVar q
  return $ server mvar

serverSpec :: Spec
serverSpec = with app $ do

  it "on POST /raptr/foo it enqueues event and returns 200" $ do
    let msg :: Message Value = MRequestVote $ RequestVote term0 "foo" index0 term0
        ev = EMessage "foo" msg
    post "/raptr/foo" (encode msg) `shouldRespondWith` ResponseMatcher { matchStatus = 200
                                                                       , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
                                                                       , matchBody = Just $ encode ev }

