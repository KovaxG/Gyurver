{-# LANGUAGE BangPatterns #-}
module Types.Video (Video(..), videosToJson, videoToJson, videoDecoder) where

import           Component.Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Types.Date

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

videoDecoder :: Int -> Decoder Video
videoDecoder nr =
  Video nr
    <$> Decoder.field "url" Decoder.string
    <*> Decoder.field "title" Decoder.string
    <*> Decoder.field "author" Decoder.string
    <*> Decoder.field "date" dateDecoder
    <*> Decoder.field "comment" Decoder.string
    <*> Decoder.field "watchDate" (Decoder.maybe dateDecoder)
    <*> Decoder.field "tags" (Decoder.list Decoder.string)
