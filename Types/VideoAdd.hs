{-# LANGUAGE OverloadedStrings #-}

module Types.VideoAdd (Request, toVideo, decoder) where

import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Data.Function ((&))
import           Data.Text (Text)
import           Types.Date (Date)
import qualified Types.Date as Date
import           Types.Video (Video)
import qualified Types.Video as Video
import           Types.Settings (Settings)
import qualified Types.Settings as Settings

data Request =  Request
  { link :: Text
  , title :: Text
  , channel :: Text
  , date :: Date
  , comment :: Text
  , watchDate :: Maybe Date
  , tags :: [Text]
  , password :: Text
  } deriving (Show)

toVideo :: Settings -> Request -> Either Text (Int -> Video)
toVideo settings request =
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
  else Left $ "Incorrect Password: \"" <> (request & password) <> "\""

decoder :: Decoder Request
decoder = Request
  <$> Decoder.field "url" Decoder.string
  <*> Decoder.field "title" Decoder.string
  <*> Decoder.field "author" Decoder.string
  <*> Decoder.field "date" Date.decoder
  <*> Decoder.field "comment" Decoder.string
  <*> Decoder.field "watchDate" (Decoder.maybe Date.decoder)
  <*> Decoder.field "tags" (Decoder.list Decoder.string)
  <*> Decoder.field "password" Decoder.string
