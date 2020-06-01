module Types.VideoAddRequest (VideoAddRequest, videoRequestToVideo, videoRequestDecoder) where

import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import Component.Json
import Types.Date
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

videoRequestToVideo :: Settings -> VideoAddRequest -> Maybe Video
videoRequestToVideo settings var =
  if Settings.password settings == password var
  then Just $
    Video.Video (link var)
                (title var)
                (channel var)
                (date var)
                (comment var)
                (watchDate var)
                (tags var)
  else Nothing

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
