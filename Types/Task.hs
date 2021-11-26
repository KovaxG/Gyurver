{-# LANGUAGE OverloadedStrings #-}

module Types.Task where

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Component.Database (DBFormat(..))
import           Types.Date (Date(..))
import qualified Types.Date as Date
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Utils

data Task = Task
  { tid :: Text
  , descr :: Text
  , timeCreated :: Int
  , timeDone :: Maybe Int
  } deriving (Eq)

toJson :: Task -> Json
toJson task = JsonObject
  [ ("id", JsonString (tid task))
  , ("descr", JsonString (descr task))
  , ("timeCreated", JsonNumber (fromIntegral $ timeCreated task))
  , ("timeDone", Json.nullable (JsonNumber . fromIntegral) (timeDone task))
  ]

fromJson :: Decoder Task
fromJson =
  Task
    <$> Decoder.field "id" Decoder.string
    <*> Decoder.field "descr" Decoder.string
    <*> Decoder.field "timeCreated" Decoder.int
    <*> Decoder.field "timeDone" (Decoder.maybe Decoder.int)

instance DBFormat Task where
  encode = Json.toString . toJson
  decode = Utils.eitherToMaybe . (=<<) (Decoder.run fromJson) . Json.parseJson
