{-# LANGUAGE OverloadedStrings #-}

module Types.Movie where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Printf (printf)
import           Data.Monoid ((<>))
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import           Types.Date (Date(..))
import qualified Types.Date as Date
import           Utils ((+:))
import qualified Utils

data MovieDiff
  = NewMovie Date Text
  | ToggleWatched Date Text
  | Delete Date Text
  deriving (Show)

diffName :: MovieDiff -> Text
diffName d = case d of
  NewMovie _ n -> n
  ToggleWatched _ n -> n
  Delete _ n -> n

data Movie = Movie
  { title :: Text
  , watched :: Bool
  , added :: Date
  , lastDiff :: Maybe Date
  } deriving (Show)

toggleWatched :: Date -> Movie -> Movie
toggleWatched d m = m { watched = not $ watched m, lastDiff = Just d }

combineDiffs :: [MovieDiff] -> [Movie]
combineDiffs = foldl update []

update :: [Movie] -> MovieDiff -> [Movie]
update ms d = case d of
  NewMovie d n -> if n `elem` map title ms then ms else ms +: Movie { title = n, watched = False, added = d, lastDiff = Nothing }
  ToggleWatched d n -> Utils.mapIf (\m -> title m == n) (toggleWatched d) ms
  Delete _ n -> Utils.filterNot (\m -> title m == n) ms

toJson :: [Movie] -> Json
toJson = JsonArray . map movieToJson
  where
    movieToJson :: Movie -> Json
    movieToJson m =
      JsonObject
        [ ("title", JsonString $ title m)
        , ("watched", JsonBool $ watched m)
        , ("added", Date.toJson $ added m)
        , ("lastModified", Json.nullable Date.toJson $ lastDiff m)
        ]

instance DBFormat MovieDiff where
  encode d = date <> prefix <> diffName d
    where
      date :: Text
      date = case d of
        NewMovie d _ -> dateToText d
        ToggleWatched d _ -> dateToText d
        Delete d _ -> dateToText d

      prefix :: Text
      prefix = case d of
        NewMovie _ _ ->  "NM"
        ToggleWatched _ _ -> "TW"
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
            "TW" -> Just $ ToggleWatched d movie
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
