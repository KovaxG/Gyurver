{-# LANGUAGE BangPatterns #-}
module Component.Vids (Video, videosToJson, jsonToVideo) where

import Component.Date
import Component.Json

data Video =  Video
  { link :: String
  , title :: String
  , channel :: String
  , date :: Date
  , comment :: String
  , watchDate :: Maybe Date
  , tags :: [String]
  } deriving (Read, Show)

videosToJson :: [Video] -> String
videosToJson = show . JsonArray . map toJson
  where
    toJson :: Video -> Json
    toJson vid = JsonObject
      [ ("url", JsonString (link vid))
      , ("title", JsonString (title vid))
      , ("author", JsonString (channel vid))
      , ("date", dateToJson (date vid))
      , ("comment", JsonString (comment vid))
      , ("watchDate", nullable dateToJson (watchDate vid))
      , ("tags", JsonArray (map JsonString (tags vid)))
      ]

jsonToVideo :: Json -> Maybe Video
jsonToVideo json = case json of
  JsonObject
    [ ("author", JsonString author)
    , ("comment", JsonString comment)
    , ("date", JsonObject [("day", JsonNumber day), ("month", JsonNumber month), ("year", JsonNumber year)])
    , ("tags", JsonArray tagJsonArray)
    , ("title", JsonString title)
    , ("url", JsonString url)
    , ("watchDate", JsonObject [("day", JsonNumber wday), ("month", JsonNumber wmonth), ("year", JsonNumber wyear)])
    ] -> Just Video
      { link = url
      , title = title
      , channel = author
      , date = Date (round year) (round month) (round day)
      , comment = comment
      , watchDate = Just $ Date (round wyear) (round wmonth) (round wday)
      , tags = map show tagJsonArray
      }
  _ -> Nothing
