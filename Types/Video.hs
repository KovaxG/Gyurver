{-# LANGUAGE BangPatterns #-}
module Types.Video (Video(..), videosToJson) where

import Component.Json
import Types.Date

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


