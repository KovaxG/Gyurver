{-# LANGUAGE BangPatterns #-}
module Component.Database (DBHandle, getHandle, insert, insertWithIndex, repsertWithIndex, everythingList, everything, get, delete) where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as Text

import Component.Semaphore (Semaphore)
import qualified Component.Semaphore as Sem
import Utils (safeReadTextFile, safeWriteTextFile, readText, mapIf)

data DBHandle a = DBHandle
  { semaphore :: Semaphore
  , path :: String
  }

getHandle :: (Read a, Show a) => String -> IO (DBHandle a)
getHandle path = do
  sem <- Sem.new
  let newPath = "Data/" ++ path ++ ".db"
  contents <- safeReadTextFile newPath
  maybe (createFile newPath) (return . const ()) contents
  return $ DBHandle sem newPath

createFile :: String -> IO ()
createFile name = fmap
  (Maybe.fromMaybe $ error "I can't create a new file, probably you need to create a Data file.")
  (safeWriteTextFile name Text.empty)

insert :: (Read a, Show a) => DBHandle a -> a -> IO ()
insert handle a = do
  Sem.block (semaphore handle)
  appendFile (path handle) (show a ++ "\n")
  Sem.unblock (semaphore handle)

insertWithIndex :: (Read a, Show a) => DBHandle a -> (Int -> a) -> IO ()
insertWithIndex handle mkA = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  let newId = maybe 0 (length . Text.lines) raw
  appendFile (path handle) (show (mkA newId) ++ "\n")
  Sem.unblock (semaphore handle)

repsertWithIndex :: (Read a, Show a) => DBHandle a -> a -> (a -> Int) -> IO ()
repsertWithIndex handle a indexOf = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  raw & maybe Text.empty func
      & safeWriteTextFile (path handle)
  Sem.unblock (semaphore handle)
  where
    func :: Text -> Text
    func = Text.unlines
         . map (Text.pack . show)
         . mapIf (\d -> indexOf d == indexOf a) (const a)
         . map (read . Text.unpack)
         . Text.lines

everythingList :: (Read a, Show a) => DBHandle a -> IO [a]
everythingList handle = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  Sem.unblock (semaphore handle)
  raw & maybe [] (map readText . Text.lines)
      & return

everything :: (Read a, Show a) => DBHandle a -> IO (Map Int a)
everything handle = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  Sem.unblock (semaphore handle)
  return $ maybe Map.empty (Map.fromList . zip [1 ..] . map readText . Text.lines) raw

get :: (Read a, Show a) => DBHandle a -> Int -> IO (Maybe a)
get handle id = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  Sem.unblock (semaphore handle)
  return $ (Maybe.listToMaybe . drop (id-1) . take id . map readText . Text.lines) =<< raw

delete :: (Read a, Show a) => DBHandle a -> (a -> Bool) -> IO ()
delete handle pred = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  let objects = maybe [] (map readText . Text.lines) raw
  safeWriteTextFile (path handle) $ Text.unlines (Text.pack . show <$> filter (not . pred) objects)
  Sem.unblock (semaphore handle)
