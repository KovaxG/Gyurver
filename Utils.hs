module Utils where

import Control.Exception
import Data.Bifunctor
import Data.Maybe

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

toRight :: l -> Maybe r -> Either l r
toRight l = maybe (Left l) Right

toJust :: Either l r -> Maybe r
toJust = either (const Nothing) Just 

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f = bimap f id

startsWith :: String -> String -> Bool
startsWith ss s = (not $ length ss > length s) && (and $ zipWith (==) ss s)

safeReadFile :: String -> IO (Maybe String)
safeReadFile path = do
  a <- try (readFile path) :: IO (Either IOException String)
  return $ case a of
    Right bla -> Just bla
    Left _ -> Nothing

safeWriteFile :: String -> String -> IO (Maybe ())
safeWriteFile path content = do
  a <- try (writeFile path content) :: IO (Either IOException ())
  return $ case a of
    Right bla -> Just bla
    Left _ -> Nothing

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . listToMaybe . reads

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e ma = case ma of
  Just a -> Right a
  Nothing -> Left e
