module Types.Video (Video(..), videosToJson, videoToJson) where

import           Component.Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Component.Database (DBFormat(..))
import           Types.Date
import qualified Data.Text as Text
import           Utils (eitherToMaybe)

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
  , ("date", dateToJson (date vid))
  , ("comment", JsonString (comment vid))
  , ("watchDate", nullable dateToJson (watchDate vid))
  , ("tags", JsonArray (map JsonString (tags vid)))
  ]

videoDecoder :: Decoder Video
videoDecoder =
  Video
    <$> Decoder.field "nr" Decoder.int
    <*> Decoder.field "url" Decoder.string
    <*> Decoder.field "title" Decoder.string
    <*> Decoder.field "author" Decoder.string
    <*> Decoder.field "date" dateDecoder
    <*> Decoder.field "comment" Decoder.string
    <*> Decoder.field "watchDate" (Decoder.maybe dateDecoder)
    <*> Decoder.field "tags" (Decoder.list Decoder.string)

instance DBFormat Video where
  encode = Text.pack . show . videoToJson
  decode =  eitherToMaybe . (=<<) (Decoder.run videoDecoder) . parseJson . Text.unpack
