{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Language where

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))

data Language = EN | HU | RO | DE deriving (Eq, Read, Show)

toString :: Language -> Text
toString lang = case lang of
  EN -> "EN"
  HU -> "HU"
  RO -> "RO"
  DE -> "DE"

toJson :: Language -> Json
toJson = JsonString . toString

fromJson :: Decoder Language
fromJson = Decoder.string >>= \case
  "EN" -> Decoder.success EN
  "RO" -> Decoder.success RO
  "DE" -> Decoder.success DE
  "HU" -> Decoder.success HU
  other -> Decoder.failure $ other <>" is not a valid language!"
