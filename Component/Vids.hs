{-# LANGUAGE BangPatterns #-}
module Component.Vids (Video, videosToJson, jsonToVideo) where

import Component.Date
import Component.Json
import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import Utils (toJust)

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
jsonToVideo = toJust . Decoder.run videoDecoder

videoDecoder :: Decoder Video
videoDecoder =
  Video <$> Decoder.field "url" Decoder.string  
        <*> Decoder.field "title" Decoder.string
        <*> Decoder.field "author" Decoder.string
        <*> Decoder.field "date" dateDecoder
        <*> Decoder.field "comment" Decoder.string
        <*> Decoder.field "watchDate" (Decoder.maybe dateDecoder)
        <*> Decoder.field "tags" (Decoder.list Decoder.string)
