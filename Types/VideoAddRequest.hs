module Types.VideoAddRequest (VideoAddRequest, videoRequestToVideo, videoRequestDecoder) where

import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import Data.Function ((&))
import Types.Date (dateDecoder, Date)
import Types.Video (Video)
import qualified Types.Video as Video
import Types.Settings (Settings)
import qualified Types.Settings as Settings


data VideoAddRequest =  VideoAddRequest
  { link :: String
  , title :: String
  , channel :: String
  , date :: Date
  , comment :: String
  , watchDate :: Maybe Date
  , tags :: [String]
  , password :: String
  } deriving (Show)

videoRequestToVideo :: Settings -> VideoAddRequest -> Either String (Int -> Video)
videoRequestToVideo settings request =
  if (settings & Settings.password)  == (request & password)
  then Right $ \vid ->
    Video.Video vid
    (request & link)
    (request & title)
    (request & channel)
    (request & date)
    (request & comment)
    (request & watchDate)
    (request & tags)
  else Left("Incorrect Password: \"" ++ (request & password) ++ "\"")

videoRequestDecoder :: Decoder VideoAddRequest
videoRequestDecoder =
  VideoAddRequest <$> Decoder.field "url" Decoder.string
                  <*> Decoder.field "title" Decoder.string
                  <*> Decoder.field "author" Decoder.string
                  <*> Decoder.field "date" dateDecoder
                  <*> Decoder.field "comment" Decoder.string
                  <*> Decoder.field "watchDate" (Decoder.maybe dateDecoder)
                  <*> Decoder.field "tags" (Decoder.list Decoder.string)
                  <*> Decoder.field "password" Decoder.string
