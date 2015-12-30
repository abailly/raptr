{-# LANGUAGE RecordWildCards #-}
module System.IO.Storage where

import qualified Data.Binary                as B
import           Data.Binary.Get
import           Data.Binary.Put            (putWord32be, runPut)
import           Data.ByteString            (ByteString, hPut, length)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Network.Kontiki.Raft       as Raft
import           Network.Raptr.Types
import           Prelude                    hiding (length)
import           System.IO                  (IOMode (..), withBinaryFile)

data FileLog = FileLog { logName :: FilePath }

insertEntry :: FileLog -> Entry Value -> IO ()
insertEntry FileLog{..} e = withBinaryFile logName AppendMode $ \ h -> do
  let bs = LBS8.toStrict $ runPut $ B.put e
      ln = LBS8.toStrict $ runPut $ putWord32be $ (fromIntegral $ length bs) + 4
  hPut h ln  -- Size of entry, including the 4 bytes of size itself
  hPut h bs  -- payload

getEntryAt :: B.Word64 -> Get (Maybe (Entry Value))
getEntryAt idx = do
  empty <- isEmpty
  if empty
    then return Nothing
    else do
    ln <- getWord32be
    bs <- getLazyByteString (fromIntegral ln)
    if idx == 0
      then return $ Just $ B.decode bs
      else getEntryAt (idx - 1)

getEntry :: FileLog -> Index -> IO (Maybe (Entry Value))
getEntry FileLog{..} idx =  withBinaryFile logName ReadMode $ \ h -> do
  bs <- LBS8.hGetContents h

  return $ runGet (getEntryAt $ unIndex idx) bs

getLastEntryFromFile ::  Maybe (Entry Value) -> Get (Maybe (Entry Value))
getLastEntryFromFile last = do
  empty <- isEmpty
  if empty
    then return last
    else do
    ln <- getWord32be
    bs <- getLazyByteString (fromIntegral ln)
    getLastEntryFromFile (Just $ B.decode bs)


getLastEntry :: FileLog -> IO (Maybe (Entry Value))
getLastEntry FileLog{..} =  withBinaryFile logName ReadMode $ \ h -> do
  bs <- LBS8.hGetContents h
  return $ runGet (getLastEntryFromFile Nothing) bs
