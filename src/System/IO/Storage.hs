{-# LANGUAGE RecordWildCards #-}
module System.IO.Storage where

import qualified Data.Binary                as B
import           Data.Binary.Put            (putWord32be, runPut)
import           Data.ByteString            (ByteString, hPut, length)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Network.Kontiki.Raft       as Raft
import           Network.Raptr.Types
import           Prelude                    hiding (length)
import           System.IO                  (IOMode (AppendMode),
                                             withBinaryFile)

data FileLog = FileLog { logName :: FilePath }

insertEntry :: FileLog -> Entry Value -> IO ()
insertEntry FileLog{..} e = withBinaryFile logName AppendMode $ \ h -> do
  let bs = LBS8.toStrict $ runPut $ B.put e
      ln = LBS8.toStrict $ runPut $ putWord32be $ (fromIntegral $ length bs) + 4
  hPut h ln  -- Size of entry, including the 4 bytes of size itself
  hPut h bs  -- payload
