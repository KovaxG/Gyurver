module Component.Date where

import Component.Json

data Date = Date Int Int Int deriving (Read, Show)

dateToJson :: Date -> Json
dateToJson (Date y m d) = JsonObject
  [ ("year", JsonNumber (fromIntegral y))
  , ("month", JsonNumber (fromIntegral m))
  , ("day", JsonNumber (fromIntegral d))
  ]

