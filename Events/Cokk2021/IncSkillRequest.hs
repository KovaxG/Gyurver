module Events.Cokk2021.IncSkillRequest where

import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Utils

data IncSkillRequest = ISK
  { username :: String
  , password :: String
  , skill :: String
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
  encode = Text.pack . show . Events.Cokk2021.IncSkillRequest.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.IncSkillRequest.decode)
    . Json.parseJson
    . Text.unpack
