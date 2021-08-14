{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.WaterRequest where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Events.Cokk2021.User (User(User))
import qualified Events.Cokk2021.User as User
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Types.DateTime (DateTime)
import qualified Types.DateTime as DateTime

data WaterRequest = WaterRequest
  { source :: Text
  , target :: Text
  , sourcePass :: Text
  }

decode :: Decoder WaterRequest
decode =
  WaterRequest <$> Decoder.field "username" Decoder.string
               <*> Decoder.field "target" Decoder.string
               <*> Decoder.field "password" Decoder.string

isWaterable :: DateTime -> DateTime -> Bool
isWaterable date@(DateTime.DateTime y1 m1 d1 _ _ _) now@(DateTime.DateTime y2 m2 d2 _ _ _) = y1 < y2 || m1 < m2 || d1 < d2
