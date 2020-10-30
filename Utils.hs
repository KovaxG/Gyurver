module Utils where

import Control.Exception
import Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Maybe

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f = bimap f id

startsWith :: String -> String -> Bool
startsWith ss s = (not $ length ss > length s) && (and $ zipWith (==) ss s)

safeReadTextFile :: String -> IO (Maybe String)
safeReadTextFile path = do
  contents <- try (readFile path) :: IO (Either IOException String)
  return $ eitherToMaybe contents

safeWriteTextFile :: String -> String -> IO (Maybe ())
safeWriteTextFile path content = do
  result <- try (writeFile path content) :: IO (Either IOException ())
  return $ eitherToMaybe result

safeReadBinaryFile :: String -> IO (Maybe ByteString)
safeReadBinaryFile path = do
  contents <- try (ByteString.readFile path) :: IO (Either IOException ByteString)
  return $ eitherToMaybe contents

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . listToMaybe . reads

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just