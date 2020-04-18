{-# LANGUAGE BangPatterns #-}
module Component.Database (DBHandle, getHandle, insert, everythingList, everything, get) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)

import Component.Semaphore (Semaphore)
import Component.Semaphore as Sem
import Utils (safeReadFile, safeWriteFile)

data DBHandle a = DBHandle
  { semaphore :: Semaphore
  , path :: String
  }

getHandle :: String -> IO (DBHandle a)
getHandle path = do
  sem <- Sem.new
  let newPath = "Data/" ++ path ++ ".db"
  contents <- safeReadFile newPath
  maybe (createFile newPath) (return . const ()) contents
  return $ DBHandle sem newPath

createFile :: String -> IO ()
createFile name = fmap 
  (fromMaybe $ error "I can't create a new file, probably you need to create a Data file.")
  (safeWriteFile name "")

insert :: (Read a, Show a) => DBHandle a -> a -> IO ()
insert handle a = do
  Sem.block (semaphore handle)
  appendFile (path handle) (show a ++ "\n")
  Sem.unblock (semaphore handle)
  
everythingList :: (Read a, Show a) => DBHandle a -> IO [a]
everythingList handle = do
  Sem.block (semaphore handle)
  !raw <- safeReadFile (path handle)
  Sem.unblock (semaphore handle)
  return $ maybe [] (map read . lines) raw

everything :: (Read a, Show a) => DBHandle a -> IO (Map Int a)
everything handle = do
  Sem.block (semaphore handle)
  !raw <- safeReadFile (path handle)
  Sem.unblock (semaphore handle)
  return $ maybe Map.empty (Map.fromList . zip [1 ..] . map read . lines) raw
  
get :: (Read a, Show a) => DBHandle a -> Int -> IO (Maybe a)
get handle id = do
  Sem.block (semaphore handle)
  !raw <- safeReadFile (path handle)
  Sem.unblock (semaphore handle)
  return $ (listToMaybe . drop (id-1) . take id . map read . lines) =<< raw
