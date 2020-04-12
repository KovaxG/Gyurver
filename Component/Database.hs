module Component.Database (DB, newDB, readDB, writeDB) where
-- appendDB
import Control.Concurrent.MVar

import Utils

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

readDB :: Read a => DB a -> IO [a]
readDB db = do
  takeMVar (semaphore db)
  raw <- safeReadFile (path db)
  putMVar (semaphore db) ()
  return $ maybe [] read raw

writeDB :: Show a => DB a -> [a] -> IO ()
writeDB db as = do
  takeMVar (semaphore db)
  result <- safeWriteFile (path db) (show as)
  putMVar (semaphore db) ()
  maybe (return ()) return result
