{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.Item where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import qualified Component.Database as DB
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Utils

data Item = Item
  { index :: Int
  , name :: Text
  , image :: Text
  , cost :: Int
  } deriving (Show)

encode :: Item -> Json
encode item = JsonObject
  [ ("index", JsonNumber $ fromIntegral $ index item)
  , ("name", JsonString $ name item)
  , ("image", JsonString $ image item)
  , ("cost", JsonNumber $ fromIntegral $ cost item)
  ]

decode :: Decoder Item
decode =
  Item <$> Decoder.field "index" Decoder.int
       <*> Decoder.field "name" Decoder.string
       <*> Decoder.field "image" Decoder.string
       <*> Decoder.field "cost" Decoder.int

instance DBFormat Item where
  encode = Json.toString . Events.Cokk2021.Item.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.Item.decode)
    . Json.parseJson

-- TODO just take the first item in the DB
initialBase :: Item
initialBase = Item
  { index = 0
  , name = "Pucér"
  , image = "https://i.postimg.cc/050KpBJb/pp.png"
  , cost = 2
  }
