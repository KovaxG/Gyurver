module Utils where

import Control.Exception
import Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

(</>) :: String -> String -> String
a </> b = a ++ "/" ++ b

infixl 1 +:
(+:) :: [a] -> a -> [a]
as +: a = as ++ [a]

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft = first

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf p f = map (\a -> if p a then f a else a)

startsWith :: String -> String -> Bool
startsWith ss s = (length ss <= length s) && and (zipWith (==) ss s)

safeReadTextFile :: String -> IO (Maybe Text)
safeReadTextFile path = do
  contents <- try (IO.readFile path) :: IO (Either IOException Text)
  return $ eitherToMaybe contents

safeWriteTextFile :: FilePath -> Text -> IO (Maybe ())
safeWriteTextFile path content = do
  result <- try (IO.writeFile path content) :: IO (Either IOException ())
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

fromRight :: a -> Either b a -> a
fromRight _ (Right a) = a
fromRight a _ = a

readText :: Read a => Text -> a
readText = read . Text.unpack

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

finiteIterateM :: Monad m => (s -> m (Either r s)) -> s -> m r
finiteIterateM f s0 = f s0 >>= either return (finiteIterateM f)
