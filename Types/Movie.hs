{-# LANGUAGE OverloadedStrings #-}

module Types.Movie where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Component.Json (Json(..))
import           Component.Database (DBFormat(..))
import           Utils ((+:))
import qualified Utils

data MovieDiff
  = NewMovie Text
  | SetWatched Text Bool
  | Delete Text
  deriving (Show)

diffName :: MovieDiff -> Text
diffName d = case d of
  NewMovie n -> n
  SetWatched n _ -> n
  Delete n -> n

data Movie = Movie Text Bool deriving (Show)

movieName :: Movie -> Text
movieName (Movie n _) = n

setWatched :: Bool -> Movie -> Movie
setWatched b (Movie n _) = Movie n b

combineDiffs :: [MovieDiff] -> [Movie]
combineDiffs = foldl update []

update :: [Movie] -> MovieDiff -> [Movie]
update ms d = case d of
  NewMovie n -> if n `elem` map movieName ms then ms else ms +: Movie n False
  SetWatched n b -> Utils.mapIf (\m -> movieName m == n) (setWatched b) ms
  Delete n -> Utils.filterNot (\m -> movieName m == n) ms

toJson :: [Movie] -> Json
toJson = JsonArray . map movieToJson
  where
    movieToJson :: Movie -> Json
    movieToJson (Movie name watched) = JsonObject [("title", JsonString name), ("watched", JsonBool watched)]

instance DBFormat MovieDiff where
  encode d = prefix <> diffName d
    where
      prefix :: Text
      prefix = case d of
        NewMovie _ ->  "NM"
        SetWatched _ True -> "WT"
        SetWatched _ False -> "WF"
        Delete _ -> "DM"

  decode s = case Text.splitAt 2 s of
    ("NM", n) -> Just $ NewMovie n
    ("WT", n) -> Just $ SetWatched n True
    ("WF", n) -> Just $ SetWatched n False
    ("DM", n) -> Just $ Delete n
    _ -> Nothing
