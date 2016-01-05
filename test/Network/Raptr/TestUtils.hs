{-# LANGUAGE ScopedTypeVariables #-}
module Network.Raptr.TestUtils
       (removeFileSafely, removeFiles)
       where

import           Control.Exception (IOException, catch)
import           Control.Monad     (mapM_)
import           System.Directory  (removeFile)
import           System.FilePath   (FilePath)

removeFileSafely :: FilePath -> IO ()
removeFileSafely path = removeFile path `catch` \ (e :: IOException) -> return ()

removeFiles :: [ FilePath ] -> IO ()
removeFiles = mapM_ removeFileSafely
