module Types.Time where

import Component.Json (Json(JsonNumber, JsonObject))
import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Data.Time as Time

data Time = Time Int Int Int deriving (Read, Show)

toJson :: Time -> Json
toJson (Time h m s) = JsonObject
  [ ("h", JsonNumber (fromIntegral h))
  , ("m", JsonNumber (fromIntegral m))
  , ("s", JsonNumber (fromIntegral s))
  ]

decoder :: Decoder Time
decoder =
  Time <$> Decoder.field "h" Decoder.int
       <*> Decoder.field "m" Decoder.int
       <*> Decoder.field "s" Decoder.int

getCurrentTime :: IO Time
getCurrentTime = do
  now <- Time.getCurrentTime
  timezone <- Time.getCurrentTimeZone
  let localTime = Time.utcToLocalTime timezone now
  let timeOfDay = Time.localTimeOfDay localTime
  return $ Time (Time.todHour timeOfDay) (Time.todMin timeOfDay) (floor $ Time.todSec timeOfDay)
