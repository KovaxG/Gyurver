{-
  Ideas for improvement:

  1. Replace the lock with STM transactions like described in the video:
     https://www.youtube.com/watch?v=2lll2VbX8Vc

  2. Remove unused functions and try to reduce the number of functions if possible.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Component.Database (DBHandle, getHandle, insert, insertWithIndex, repsertWithIndex, everythingList, everything, get, delete, modifyData, DBFormat(..)) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Function ((&))
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Data.Monoid ((<>))

import           Component.Semaphore (Semaphore)
import qualified Component.Semaphore as Sem
import Utils (safeReadTextFile, safeWriteTextFile, readText, mapIf)

class DBFormat a where
  encode :: a -> Text
  decode :: Text -> Maybe a

instance DBFormat Text where
  encode = id
  decode = Just

data DBHandle a = DBHandle
  { semaphore :: Semaphore
  , path :: Text
  }

getHandle :: DBFormat a => Text -> IO (DBHandle a)
getHandle path = do
  sem <- Sem.new
  let newPath = "Data/" <> path <> ".db"
  contents <- safeReadTextFile newPath
  maybe (createFile newPath) (return . const ()) contents
  return $ DBHandle sem newPath

createFile :: Text -> IO ()
createFile name = fmap
  (Maybe.fromMaybe $ error "I can't create a new file, probably you need to create a Data file.")
  (safeWriteTextFile name Text.empty)

insert :: DBFormat a => DBHandle a -> a -> IO ()
insert handle a = do
  Sem.block (semaphore handle)
  TIO.appendFile (Text.unpack $ path handle) (Text.snoc (encode a) '\n')
  Sem.unblock (semaphore handle)

insertWithIndex :: DBFormat a => DBHandle a -> (Int -> a) -> (a -> Int) -> IO ()
insertWithIndex handle mkA index= do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  let newId = maybe 0 ((+1) . maximum . (:) 0 . map (index . Maybe.fromJust . decode) . Text.lines) raw
  TIO.appendFile (Text.unpack $ path handle) (Text.snoc (encode $ mkA newId) '\n')
  Sem.unblock (semaphore handle)

repsertWithIndex :: DBFormat a => DBHandle a -> a -> (a -> Int) -> IO ()
repsertWithIndex handle a indexOf = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  raw & maybe Text.empty modifyData
      & safeWriteTextFile (path handle)
  Sem.unblock (semaphore handle)
  where
    modifyData :: Text -> Text
    modifyData =
      Text.unlines
      . map encode
      . mapIf (\d -> indexOf d == indexOf a) (const a)
      . map (Maybe.fromJust . decode)
      . Text.lines

everythingList :: DBFormat a => DBHandle a -> IO [a]
everythingList handle = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  Sem.unblock (semaphore handle)
  return $ maybe [] (map (Maybe.fromJust . decode) . Text.lines) raw

everything :: DBFormat a => DBHandle a -> IO (Map Int a)
everything handle = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  Sem.unblock (semaphore handle)
  return $ maybe Map.empty (Map.fromList . zip [1 ..] . map (Maybe.fromJust . decode) . Text.lines) raw

get :: DBFormat a => DBHandle a -> Int -> IO (Maybe a)
get handle index = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  Sem.unblock (semaphore handle)
  return $ (Maybe.listToMaybe . drop (index - 1) . take index . map (Maybe.fromJust . decode) . Text.lines) =<< raw

delete :: DBFormat a => DBHandle a -> (a -> Bool) -> IO ()
delete handle pred = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  let objects = maybe [] (map (Maybe.fromJust . decode) . Text.lines) raw
  safeWriteTextFile (path handle) $ Text.unlines (encode <$> filter (not . pred) objects)
  Sem.unblock (semaphore handle)

modifyData :: DBFormat a => DBHandle a -> ([a] -> ([a], b)) -> IO b
modifyData handle f = do
  Sem.block (semaphore handle)
  !raw <- safeReadTextFile (path handle)
  let objects = maybe [] (map (Maybe.fromJust . decode) . Text.lines) raw
  let (newObjects, result) = f objects
  safeWriteTextFile (path handle) $ Text.unlines $ fmap encode newObjects
  Sem.unblock (semaphore handle)
  return result
