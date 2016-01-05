{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
-- | A basic storage engine for @Raptr@ which stores commands in a flat file.
--
-- This modules exposes a simple Raft persistent log implementation which stores entries in binary
-- format in a flat file, with the following structure:
--
--      +----+----+----+----+
--      | 78 | 56 | 34 | 12 |  4 bytes MAGIC header
--      +----+----+----+----+
--      |  length of entry  |  4 bytes length of entry
--      +----+--------------+
--      | Vr.| CRC (0-2)    |  1 byte version
--      +----+--------------+
--      |CRC | payload      |  4 bytes CRC
--      +-------------------+
--      |  payload          |  payload of len (length - 5)
--      |                   |
--           ...
--      |                   |
--      +-------------------+
--      | <next entry>      |
--         ...
--
module System.IO.Storage
       ( -- * Types
         FileLog(..), magic, version,
         -- * Management
         openLog,
         -- * Raft Operations
         insertEntry, getEntry, getLastEntry, truncateLogAtIndex,
         -- * Other Operations
         getAllData)
       where

import           Control.DeepSeq
import           Control.Exception          (throw)
import           Control.Monad              (when)
import qualified Data.Binary                as B
import           Data.Binary.Get
import           Data.Binary.Put            (putWord32be, putWord8, runPut)
import           Data.ByteString            (ByteString, hPut, length)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Monoid                ((<>))
import           Network.Kontiki.Raft       as Raft
import           Network.Raptr.Types
import           Prelude                    hiding (length)
import qualified Prelude                    as P
import           System.Directory           (doesDirectoryExist, doesFileExist)
import           System.IO                  (IOMode (..), hClose, hFlush,
                                             hSetFileSize, openBinaryFile,
                                             withBinaryFile)

-- | Representation of Raft log on disk.
data FileLog = FileLog { logName :: FilePath }


-- | Magic header for raptr log files.
magic :: B.Word32
magic = 0x12345678

-- |Current version number
version :: B.Word8
version = 1

skipMagic :: Get ()
skipMagic = do
  mag <- getWord32be
  if mag /= magic
    then fail "incorrect file format, cannot read magic marker at beginning of file"
    else return ()

trace :: FileLog -> String -> IO ()
trace FileLog{..} s = putStrLn $ "[" <> logName <> "] - " <> s

-- | Open a @FileLog@ for use by a Raptr node
--
-- The log file is created if it does not exist as well as any intermediary directory as
-- given by 'path'.
--
-- Returns an initialized 'FileLog' or throw a 'UserError' if file cannot be created or is a
-- directory.
openLog :: FilePath -> IO FileLog
openLog path = do
  exist <- doesFileExist path
  isdir <- doesDirectoryExist path
  when isdir $ throw $ userError ("log file " ++ path ++ " is a directory")
  when (not exist) $ withBinaryFile path WriteMode $ \ h ->
    hPut h $ LBS8.toStrict $ runPut $ putWord32be magic
  let log = FileLog path
  trace log "opened log"
  return log

-- | Insert a new 'Entry' at end of given 'FileLog'
--
-- The entry's index is asssumed to be correctly set by caller.
insertEntry :: FileLog -> Entry Value -> IO ()
insertEntry log@FileLog{..} e = withBinaryFile logName AppendMode $ \ h -> do
  trace log $ "inserting entry: " <> show e
  let bs = LBS8.toStrict $ runPut $ B.put e
      ln = LBS8.toStrict $ runPut $ putWord32be $ (fromIntegral $ length bs) + 5
  hPut h ln                     -- Size of entry, excluding the 4 bytes of length
  hPut h $ LBS8.toStrict
    $ runPut $ putWord8 version -- version
  hPut h $ LBS8.toStrict
    $ runPut $ putWord32be 0    -- TODO compute real CRC
  hPut h bs                     -- payload
  trace log "inserted entry"


readSingleEntry :: Get (B.Word32, LBS8.ByteString)
readSingleEntry = do
  ln  <- getWord32be
  v   <- getWord8
  crc <- getWord32be
  bs  <- getLazyByteString (fromIntegral ln - 5)
  return (ln, bs)

readUntilEntry :: B.Word64 -> Integer -> Get Integer
readUntilEntry 0   pos = return pos
readUntilEntry idx pos = do
  empty <- isEmpty
  if empty
    then return pos
    else do
    (ln, bs) <- readSingleEntry
    readUntilEntry (idx - 1) (pos + fromIntegral ln + 4)

readTruncatePosition :: FilePath -> Index -> IO Integer
readTruncatePosition logName idx = withBinaryFile logName ReadWriteMode $ \ h -> do
  bs <- LBS8.hGetContents h
  let !pos = runGet (skipMagic >> readUntilEntry (unIndex idx) 0) bs
  hClose h
  return pos

-- | Truncate 'FileLog' to given number of entries.
-- If 'idx' is greater than or equal than number of entries, file is unchanged.
truncateLogAtIndex :: FileLog -> Index -> IO ()
truncateLogAtIndex log@FileLog{..} idx = do
  trace log $ "truncating at index: " <> show idx
  !truncateAt <- readTruncatePosition logName idx
  trace log $ "truncating at actual position: " <> show truncateAt
  h <- openBinaryFile logName ReadWriteMode
  hSetFileSize h (truncateAt + 4)
  hClose h
  trace log $ "truncated log"

getEntryAt :: B.Word64 -> Get (Maybe (Entry Value))
getEntryAt 0 = return Nothing
getEntryAt 1 = do
  empty <- isEmpty
  if empty
    then return Nothing
    else (Just . B.decode . snd) <$> readSingleEntry
getEntryAt idx = do
  empty <- isEmpty
  if empty
    then return Nothing
    else getEntryAt (idx - 1)

-- | Tries to retrieve 'Entry' at given index.
--
-- Returns 'Just entry' if found, otherwise 'Nothing'
getEntry :: FileLog -> Index -> IO (Maybe (Entry Value))
getEntry log@FileLog{..} idx =  withBinaryFile logName ReadMode $ \ h -> do
  trace log $ "getting entry at index: " <> show idx
  bs <- LBS8.hGetContents h
  let e = runGet (skipMagic >> getEntryAt (unIndex idx)) bs
  trace log $ "got entry " ++ show e
  return e

getLastEntryFromFile ::  Maybe (Entry Value) -> Get (Maybe (Entry Value))
getLastEntryFromFile last = do
  empty <- isEmpty
  if empty
    then return last
    else do
    (Just . B.decode . snd) <$> readSingleEntry >>= getLastEntryFromFile

-- | Tries to retrieve last 'Entry'.
--
-- Returns 'Just entry' if log is not empty, 'Nothing' otherwise.
getLastEntry :: FileLog -> IO (Maybe (Entry Value))
getLastEntry log@FileLog{..} =  withBinaryFile logName ReadMode $ \ h -> do
  trace log "getting last entry"
  bs <- LBS8.hGetContents h
  let e = runGet (skipMagic >> getLastEntryFromFile Nothing) bs
  trace log $ "got last entry: " ++ show e
  return e

readAllData :: Get [ LBS8.ByteString ]
readAllData = do
  empty <- isEmpty
  if empty
    then return []
    else do
    (_,bs) <- readSingleEntry
    (eValue (B.decode bs):) <$> readAllData

-- | Returns a (lazy) list of (lazy) 'ByteString' of all data stored within
-- entries in this log file.
--
-- **Note**: It is probably a good idea to consume the returned list otherwise the underlying file
-- handle will be kept in so-called semi-closed state hence will return a lock which will prevent
-- subsequent write operations on this file to operate properly.
getAllData :: FileLog -> IO [ LBS8.ByteString ]
getAllData log@FileLog{..} = do
  trace log $ "retrieving all data"
  h <- openBinaryFile logName ReadMode
  bs <- LBS8.hGetContents h
  let pos = bs `deepseq` runGet (skipMagic >> readAllData) bs
  pos `deepseq` hClose h
  return pos

