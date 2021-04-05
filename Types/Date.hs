module Types.Date where

import Component.Json
import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Data.Time as Time
import qualified Data.Time.Calendar as Calendar

data Date = Date Int Int Int deriving (Read, Show, Eq)

toJson :: Date -> Json
toJson (Date y m d) = JsonObject
  [ ("year", JsonNumber (fromIntegral y))
  , ("month", JsonNumber (fromIntegral m))
  , ("day", JsonNumber (fromIntegral d))
  ]

decoder :: Decoder Date
decoder =
  Date <$> Decoder.field "year" Decoder.int
       <*> Decoder.field "month" Decoder.int
       <*> Decoder.field "day" Decoder.int

getCurrentDate :: IO Date
getCurrentDate = do
  now <- Time.getCurrentTime
  timezone <- Time.getCurrentTimeZone
  let localTime = Time.utcToLocalTime timezone now
  let day = Time.localDay localTime
  let (y, m, d) = Calendar.toGregorian day
  return $ Date (fromIntegral y) m d
