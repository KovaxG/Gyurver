module Types.Date where

import Component.Json
import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

data Date = Date Int Int Int deriving (Read, Show)

dateToJson :: Date -> Json
dateToJson (Date y m d) = JsonObject
  [ ("year", JsonNumber (fromIntegral y))
  , ("month", JsonNumber (fromIntegral m))
  , ("day", JsonNumber (fromIntegral d))
  ]

dateDecoder :: Decoder Date
dateDecoder =
  Date <$> Decoder.field "year" Decoder.int
       <*> Decoder.field "month" Decoder.int
       <*> Decoder.field "day" Decoder.int
