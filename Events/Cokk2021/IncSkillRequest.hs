{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.IncSkillRequest where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Events.Cokk2021.Login (Login (..))
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Utils

data IncSkillRequest = ISK
  { username :: Text
  , password :: Text
  , skill :: Text
  } deriving (Show)

encode :: IncSkillRequest -> Json
encode s = JsonObject
  [ ("username", JsonString $ username s)
  , ("password", JsonString $ password s)
  , ("skill", JsonString $ skill s)
  ]

decode :: Decoder IncSkillRequest
decode =
  ISK <$> Decoder.field "username" Decoder.string
      <*> Decoder.field "password" Decoder.string
      <*> Decoder.field "skill" Decoder.string

instance DBFormat IncSkillRequest where
  encode = Json.toString . Events.Cokk2021.IncSkillRequest.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.IncSkillRequest.decode)
    . Json.parseJson

toLogin :: IncSkillRequest -> Login
toLogin ISK {username, password} = Login username password
