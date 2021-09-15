{-# LANGUAGE OverloadedStrings #-}

module Types.Movie where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Printf (printf)
import           Data.Monoid ((<>))
import           Component.Json (Json(..))
import           Component.Database (DBFormat(..))
import           Types.Date (Date(..))
import           Utils ((+:))
import qualified Utils

data MovieDiff
  = NewMovie Date Text
  | SetWatched Date Bool Text
  | Delete Date Text
  deriving (Show)

diffName :: MovieDiff -> Text
diffName d = case d of
  NewMovie _ n -> n
  SetWatched _ _ n -> n
  Delete _ n -> n

data Movie = Movie Text Bool deriving (Show)

movieName :: Movie -> Text
movieName (Movie n _) = n

setWatched :: Bool -> Movie -> Movie
setWatched b (Movie n _) = Movie n b

combineDiffs :: [MovieDiff] -> [Movie]
combineDiffs = foldl update []

update :: [Movie] -> MovieDiff -> [Movie]
update ms d = case d of
  NewMovie _ n -> if n `elem` map movieName ms then ms else ms +: Movie n False
  SetWatched _ b n -> Utils.mapIf (\m -> movieName m == n) (setWatched b) ms
  Delete _ n -> Utils.filterNot (\m -> movieName m == n) ms

toJson :: [Movie] -> Json
toJson = JsonArray . map movieToJson
  where
    movieToJson :: Movie -> Json
    movieToJson (Movie name watched) = JsonObject [("title", JsonString name), ("watched", JsonBool watched)]

instance DBFormat MovieDiff where
  encode d = date <> prefix <> diffName d
    where
      date :: Text
      date = case d of
        NewMovie d _ -> dateToText d
        SetWatched d _ _ -> dateToText d
        Delete d _ -> dateToText d

      prefix :: Text
      prefix = case d of
        NewMovie _ _ ->  "NM"
        SetWatched _ True _ -> "WT"
        SetWatched _ False _ -> "WF"
        Delete _ _ -> "DM"

  decode s =
    let (prefix, movie) = Text.splitAt 10 s
        (dateRaw, cmd) = Text.splitAt 8 prefix
        date = parseDate dateRaw
    in
      case date of
        Just d ->
          case cmd of
            "NM" -> Just $ NewMovie d movie
            "WT" -> Just $ SetWatched d True movie
            "WF" -> Just $ SetWatched d False movie
            "DM" -> Just $ Delete d movie
            _ -> Nothing
        Nothing -> Nothing

parseDate :: Text -> Maybe Date
parseDate t = case Text.unpack t of
  [c1, c2, c3, c4, c5, c6, c7, c8] ->
    let year = Utils.safeRead $ Text.pack [c1,c2,c3,c4]
        month = Utils.safeRead $ Text.pack [c5, c6]
        day = Utils.safeRead $ Text.pack [c7, c8]
    in Date <$> year <*> month <*> day
  _ -> Nothing

dateToText :: Date -> Text
dateToText (Date y m d) = Text.pack $ printf "%04d%02d%02d" y m d