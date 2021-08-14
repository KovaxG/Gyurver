{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.WaterLog where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Types.DateTime (DateTime)
import qualified Types.DateTime as DateTime
import qualified Utils

data WaterLog = WaterLog
  { wlSource :: Text
  , wlTarget :: Text
  , wlDateTime :: DateTime
  }

make :: Text -> Text -> IO WaterLog
make source target = WaterLog source target <$> DateTime.getCurrentDateTime

encode :: WaterLog -> Json
encode wl = JsonObject
  [ ("source", JsonString $ wlSource wl)
  , ("target", JsonString $ wlTarget wl)
  , ("time", DateTime.toJson $ wlDateTime wl)
  ]

decode :: Decoder WaterLog
decode =
  WaterLog <$> Decoder.field "source" Decoder.string
           <*> Decoder.field "target" Decoder.string
           <*> Decoder.field "time" DateTime.decoder

instance DBFormat WaterLog where
  encode = Json.toString . Events.Cokk2021.WaterLog.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.WaterLog.decode)
    . Json.parseJson
