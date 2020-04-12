{-# LANGUAGE BangPatterns #-}
module Component.Database (DB, newDB, readDB, writeDB, appendDB) where

import Control.Concurrent.MVar

import Utils

-- TODO use safeRead instead of read!

data DB a = DB
  { semaphore :: MVar ()
  , path :: String
  }

newDB :: String -> IO (DB a)
newDB filePath = do
  sem <- newMVar ()
  return $ DB
    { semaphore = sem
    , path = filePath
    }

readDB :: (Show a, Read a) => DB a -> IO [a]
readDB db = do
  takeMVar (semaphore db)
  raw <- safeReadFile (path db)
  putMVar (semaphore db) ()
  return $ maybe [] read raw

writeDB :: (Show a, Read a) => DB a -> [a] -> IO ()
writeDB db as = do
  takeMVar (semaphore db)
  result <- safeWriteFile (path db) (show as)
  putMVar (semaphore db) ()
  maybe (return ()) return result

appendDB :: (Show a, Read a) => DB a -> a -> IO ()
appendDB db a = do
  takeMVar (semaphore db)
  !raw <- safeReadFile (path db)
  let !oldData = maybe [] read raw
  let !newData = a : oldData
  !result <- safeWriteFile (path db) (show newData)
  putMVar (semaphore db) ()
  maybe (return ()) return result