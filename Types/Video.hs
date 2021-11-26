{-# LANGUAGE OverloadedStrings #-}

module Types.Video (Video(..), videosToJson, videoToJson) where

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Component.Database (DBFormat(..))
import           Types.Date (Date(..))
import qualified Types.Date as Date
import           Data.Text (Text)
import qualified Data.Text as Text
import           Utils (eitherToMaybe)

data Video =  Video
  { nr :: Int
  , link :: Text
  , title :: Text
  , channel :: Text
  , date :: Date
  , comment :: Text
  , watchDate :: Maybe Date
  , tags :: [Text]
  } deriving (Read, Show)

testVideo :: Video
testVideo = Video
  { nr = 42
  , link  = "www.whatever.com"
  , title = "title"
  , channel = "channel"
  , date = Date 2020 12 24
  , comment = "bla bla bla"
  , watchDate = Nothing
  , tags = ["a", "b", "c"]
  }

videosToJson :: [Video] -> Json
videosToJson = JsonArray . map videoToJson

videoToJson :: Video -> Json
videoToJson vid = JsonObject
  [ ("nr", JsonNumber (fromIntegral $ nr vid))
  , ("url", JsonString (link vid))
  , ("title", JsonString (title vid))
  , ("author", JsonString (channel vid))
  , ("date", Date.toJson (date vid))
  , ("comment", JsonString (comment vid))
  , ("watchDate", Json.nullable Date.toJson (watchDate vid))
  , ("tags", JsonArray (map JsonString (tags vid)))
  ]

videoDecoder :: Decoder Video
videoDecoder =
  Video
    <$> Decoder.field "nr" Decoder.int
    <*> Decoder.field "url" Decoder.string
    <*> Decoder.field "title" Decoder.string
    <*> Decoder.field "author" Decoder.string
    <*> Decoder.field "date" Date.decoder
    <*> Decoder.field "comment" Decoder.string
    <*> Decoder.field "watchDate" (Decoder.maybe Date.decoder)
    <*> Decoder.field "tags" (Decoder.list Decoder.string)

instance DBFormat Video where
  encode = Json.toString . videoToJson
  decode = eitherToMaybe . (=<<) (Decoder.run videoDecoder) . Json.parseJson
