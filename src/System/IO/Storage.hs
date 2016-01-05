{-# LANGUAGE RecordWildCards #-}
module System.IO.Storage where

import           Control.Exception          (throw)
import           Control.Monad              (when)
import qualified Data.Binary                as B
import           Data.Binary.Get
import           Data.Binary.Put            (putWord32be, runPut)
import           Data.ByteString            (ByteString, hPut, length)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Network.Kontiki.Raft       as Raft
import           Network.Raptr.Types
import           Prelude                    hiding (length)
import           System.Directory           (doesDirectoryExist, doesFileExist)
import           System.IO                  (IOMode (..), hClose, hFlush,
                                             hSetFileSize, openBinaryFile,
                                             withBinaryFile)

data FileLog = FileLog { logName :: FilePath }

magic :: B.Word32
magic = 0x12345678

skipMagic :: Get ()
skipMagic = do
  mag <- getWord32be
  if mag /= magic
    then fail "incorrect file format, cannot read magic marker at beginning of file"
    else return ()

openLog :: FilePath -> IO FileLog
openLog path = do
  exist <- doesFileExist path
  isdir <- doesDirectoryExist path
  when isdir $ throw $ userError ("log file " ++ path ++ " is a directory")
  when (not exist) $ withBinaryFile path WriteMode $ \ h ->
    hPut h $ LBS8.toStrict $ runPut $ putWord32be magic
  return $ FileLog path

insertEntry :: FileLog -> Entry Value -> IO ()
insertEntry FileLog{..} e = withBinaryFile logName AppendMode $ \ h -> do
  let bs = LBS8.toStrict $ runPut $ B.put e
      ln = LBS8.toStrict $ runPut $ putWord32be $ (fromIntegral $ length bs)
  hPut h ln  -- Size of entry, excluding the 4 bytes of length
  hPut h bs  -- payload

readUntilEntry :: B.Word64 -> Integer -> Get Integer
readUntilEntry 0   pos = return pos
readUntilEntry idx pos = do
  empty <- isEmpty
  if empty
    then return pos
    else do
    ln <- getWord32be
    bs <- getLazyByteString (fromIntegral ln)
    readUntilEntry (idx - 1) (pos + fromIntegral ln + 4)

truncateLogAtIndex :: FileLog -> Index -> IO ()
truncateLogAtIndex FileLog{..} idx = do
  truncateAt <- withBinaryFile logName ReadWriteMode $ \ h -> do
    bs <- LBS8.hGetContents h
    let pos = runGet (skipMagic >> readUntilEntry (unIndex idx) 0) bs
    putStrLn $ "position is " ++ show pos
    return pos
  h <- openBinaryFile logName ReadWriteMode
  hSetFileSize h (truncateAt + 4)
  hClose h

getEntryAt :: B.Word64 -> Get (Maybe (Entry Value))
getEntryAt 0 = return Nothing
getEntryAt 1 = do
  empty <- isEmpty
  if empty
    then return Nothing
    else do
    ln <- getWord32be
    bs <- getLazyByteString (fromIntegral ln)
    return $ Just $ B.decode bs
getEntryAt idx = do
  empty <- isEmpty
  if empty
    then return Nothing
    else getEntryAt (idx - 1)

getEntry :: FileLog -> Index -> IO (Maybe (Entry Value))
getEntry FileLog{..} idx =  withBinaryFile logName ReadMode $ \ h -> do
  bs <- LBS8.hGetContents h
  let e = runGet (skipMagic >> getEntryAt (unIndex idx)) bs
  putStrLn $ "entry at index "++ show idx ++ " in log " ++ logName ++ ": "++ show e
  return e

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
  let e = runGet (skipMagic >> getLastEntryFromFile Nothing) bs
  putStrLn $ "last entry in log " ++ logName ++ ": "++ show e
  return e
