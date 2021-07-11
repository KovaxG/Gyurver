{-# LANGUAGE LambdaCase #-}

module Types.Language where

import           Data.Function ((&))
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

data Language = EN | HU | RO | DE

instance Show Language where
  show lang = case lang of
    EN -> "EN"
    HU -> "HU"
    RO -> "RO"
    DE -> "DE"

toJson :: Language -> Json
toJson = JsonString . show

fromJson :: Decoder Language
fromJson = Decoder.string >>= \case
  "EN" -> Decoder.success EN
  "RO" -> Decoder.success RO
  "DE" -> Decoder.success DE
  "HU" -> Decoder.success HU
  other -> Decoder.failure $ other ++ " is not a valid language!"
