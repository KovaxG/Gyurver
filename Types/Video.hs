{-# LANGUAGE BangPatterns #-}
module Types.Video (Video(..), videosToJson, videoToJson) where

import Component.Json
import Types.Date

data Video =  Video
  { nr :: Int
  , link :: String
  , title :: String
  , channel :: String
  , date :: Date
  , comment :: String
  , watchDate :: Maybe Date
  , tags :: [String]
  } deriving (Read, Show)

videosToJson :: [Video] -> Json
videosToJson = JsonArray . map videoToJson

videoToJson :: Video -> Json
videoToJson vid = JsonObject
  [ ("nr", JsonNumber (fromIntegral $ nr vid))
  , ("url", JsonString (link vid))
  , ("title", JsonString (title vid))
  , ("author", JsonString (channel vid))
  , ("date", dateToJson (date vid))
  , ("comment", JsonString (comment vid))
  , ("watchDate", nullable dateToJson (watchDate vid))
  , ("tags", JsonArray (map JsonString (tags vid)))
  ]


