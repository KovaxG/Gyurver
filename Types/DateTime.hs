{-# LANGUAGE OverloadedStrings #-}

module Types.DateTime where

import qualified Data.Time as DTime
import qualified Data.Time.Calendar as Calendar

import           Component.Json (Json(JsonNumber, JsonObject))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

import           Data.Text (Text)
import qualified Data.Text as Text
import           Types.Date (Date(..))
import qualified Types.Date as Date
import           Types.Time (Time(..))
import qualified Types.Time as Time
import qualified Utils

data DateTime = DateTime Int Int Int Int Int Double deriving (Read, Show, Eq, Ord)

jsonEncoder :: DateTime -> Json
jsonEncoder (DateTime y m d h min s) = JsonObject
  [ ("year", JsonNumber $ fromIntegral y)
  , ("month", JsonNumber $ fromIntegral m)
  , ("day", JsonNumber $ fromIntegral d)
  , ("hours", JsonNumber $ fromIntegral h)
  , ("minutes", JsonNumber $ fromIntegral min)
  , ("seconds", JsonNumber s)
  ]

jsonDecoder :: Decoder DateTime
jsonDecoder =
  DateTime <$> Decoder.field "year" Decoder.int
           <*> Decoder.field "month" Decoder.int
           <*> Decoder.field "day" Decoder.int
           <*> Decoder.field "hours" Decoder.int
           <*> Decoder.field "minutes" Decoder.int
           <*> Decoder.field "seconds" Decoder.double

textEncoder :: DateTime -> Text
textEncoder (DateTime y m d h min s) =
  Text.intercalate "-" $ fmap (Text.pack . show) [y, m, d, h, min, round s]

textDecoder :: Text -> Maybe DateTime
textDecoder txt = case Text.words $ Text.map (\c -> if c == '-' then ' ' else c) txt of
  [yt, mt, dt, ht, mint, st] -> do
    y <- Utils.safeRead yt
    m <- Utils.safeRead mt
    d <- Utils.safeRead dt
    h <- Utils.safeRead ht
    min <- Utils.safeRead mint
    s <- Utils.safeRead st
    return $ DateTime y m d h min (fromIntegral s)
  _ -> Nothing

toDate :: DateTime -> Date
toDate (DateTime y m d _ _ _) = Date y m d

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
