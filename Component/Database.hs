module Component.Database (DB, newDB, readDB, writeDB) where
-- appendDB
import Control.Concurrent.MVar

import Utils

data DB = DB 
  { semaphore :: MVar ()
  , path :: String 
  }

newDB :: String -> IO DB
newDB filePath = do
  sem <- newMVar ()
  return $ DB
    { semaphore = sem
    , path = filePath
    }

readDB :: Read a => DB -> IO [a]
readDB db = do
  putStrLn "read waiting..."
  takeMVar (semaphore db)
  putStrLn "blocking"
  raw <- safeReadFile (path db)
  putStrLn "time to release"
  putMVar (semaphore db) ()
  return $ maybe [] read raw

writeDB :: Show a => DB -> [a] -> IO ()
writeDB db as = do
  putStrLn "write waiting..."
  takeMVar (semaphore db)
  putStrLn "blocking"
  result <- safeWriteFile (path db) (show as)
  putStrLn "time to release"
  putMVar (semaphore db) ()
  maybe (putStrLn "Failed to write, trying again...")
        return
        result
  