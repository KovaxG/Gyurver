module Types.DateTime where

import qualified Data.Time as DTime
import qualified Data.Time.Calendar as Calendar

import Component.Json (Json(JsonNumber, JsonObject))
import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

import           Types.Date (Date(..))
import qualified Types.Date as Date
import           Types.Time (Time(..))
import qualified Types.Time as Time

data DateTime = DateTime Int Int Int Int Int Double deriving (Read, Show)

toJson :: DateTime -> Json
toJson (DateTime y m d h min s) = JsonObject
  [ ("year", JsonNumber $ fromIntegral y)
  , ("month", JsonNumber $ fromIntegral m)
  , ("day", JsonNumber $ fromIntegral d)
  , ("hours", JsonNumber $ fromIntegral h)
  , ("minutes", JsonNumber $ fromIntegral min)
  , ("seconds", JsonNumber s)
  ]

decoder :: Decoder DateTime
decoder =
  DateTime <$> Decoder.field "year" Decoder.int
           <*> Decoder.field "month" Decoder.int
           <*> Decoder.field "day" Decoder.int
           <*> Decoder.field "hours" Decoder.int
           <*> Decoder.field "minutes" Decoder.int
           <*> Decoder.field "seconds" Decoder.double

getCurrentDateTime :: IO DateTime
getCurrentDateTime = do
  now <- DTime.getCurrentTime
  timezone <- DTime.getCurrentTimeZone
  let localTime = DTime.utcToLocalTime timezone now
  let day = DTime.localDay localTime
  let (y, m, d) = Calendar.toGregorian day
  let timeOfDay = DTime.localTimeOfDay localTime
  return $ DateTime
    (fromIntegral y) m d
    (DTime.todHour timeOfDay) (DTime.todMin timeOfDay) (realToFrac $ DTime.todSec timeOfDay)
