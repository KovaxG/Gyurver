{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Control.Exception
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Text.IO as IO
import qualified Data.Text.Encoding as Encoding

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b

infixl 1 +:
(+:) :: [a] -> a -> [a]
as +: a = as ++ [a]

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft = first

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf p f = map (\a -> if p a then f a else a)

startsWith :: String -> String -> Bool
startsWith ss s = (length ss <= length s) && and (zipWith (==) ss s)

safeReadTextFile :: Text -> IO (Maybe Text)
safeReadTextFile path = do
  contents <- try (Encoding.decodeUtf8 <$> ByteString.readFile (Text.unpack path)) :: IO (Either IOException Text)
  return $ eitherToMaybe contents

safeWriteTextFile :: Text -> Text -> IO (Maybe ())
safeWriteTextFile path content = do
  result <- try (IO.writeFile (Text.unpack path) content) :: IO (Either IOException ())
  return $ eitherToMaybe result

safeReadBinaryFile :: String -> IO (Maybe ByteString)
safeReadBinaryFile path = do
  contents <- try (ByteString.readFile path) :: IO (Either IOException ByteString)
  return $ eitherToMaybe contents

safeRead :: Read a => Text -> Maybe a
safeRead = fmap fst . Maybe.listToMaybe . reads . Text.unpack

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

fromRight :: a -> Either b a -> a
fromRight _ (Right a) = a
fromRight a _ = a

readText :: Read a => Text -> a
readText = read . Text.unpack

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeDeHead :: [a] -> Maybe (a, [a])
safeDeHead [] = Nothing
safeDeHead (x:xs) = Just (x, xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

finiteIterateM :: Monad m => (s -> m (Either r s)) -> s -> m r
finiteIterateM f s0 = f s0 >>= either return (finiteIterateM f)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter (not . p)

dequote :: Text -> Text
dequote s
  | Text.head s == '"' && Text.last s == '"' = Text.init $ Text.tail s
  | otherwise = s

trim :: Text -> Text
trim = Text.reverse . Text.dropWhile (==' ') . Text.reverse . Text.dropWhile (==' ')

stripPrefix :: Text -> Text -> Text
stripPrefix prefix s = Maybe.fromMaybe s $ Text.stripPrefix prefix s

stripSuffix :: Text -> Text -> Text
stripSuffix suffix s = maybe s Text.reverse $ Text.stripPrefix (Text.reverse suffix) $ Text.reverse s

-- Generate footnote symbol given the footnote nr
foot :: Int -> Text
foot n = Text.replicate (nr + 1) (symbols !! index)
  where
    (nr, index) = divMod n (length symbols)
    symbols = ["*", "†", "††", "¶", "§", "||", "#"]

bracketedText :: String -> [String]
bracketedText = go []
  where
    go :: [String] -> String -> [String]
    go acc "" = acc
    go acc t  =
      let start = dropWhile (/= '(') t
          bracketed = if ')' `elem` start then takeWhile (/= ')') $ filter (/= '(') start else ""
          rest = dropWhile (/= ')') start
      in go (acc ++ [bracketed | not $ null bracketed]) rest
