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

data DateTime = DateTime Date Time deriving (Read, Show)

toJson :: DateTime -> Json
toJson (DateTime d t) = JsonObject
  [ ("date", Date.toJson d)
  , ("time", Time.toJson t)
  ]

decoder :: Decoder DateTime
decoder =
  DateTime <$> Decoder.field "date" Date.decoder
           <*> Decoder.field "time" Time.decoder

getCurrentDateTime :: IO DateTime
getCurrentDateTime = do
  now <- DTime.getCurrentTime
  timezone <- DTime.getCurrentTimeZone
  let localTime = DTime.utcToLocalTime timezone now
  let day = DTime.localDay localTime
  let (y, m, d) = Calendar.toGregorian day
  let localTime = DTime.utcToLocalTime timezone now
  let timeOfDay = DTime.localTimeOfDay localTime
  let time = Time (DTime.todHour timeOfDay) (DTime.todMin timeOfDay) (floor $ DTime.todSec timeOfDay)
  let date = Date (fromIntegral y) m d
  return $ DateTime date time
