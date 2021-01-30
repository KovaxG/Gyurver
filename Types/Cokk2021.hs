module Types.Cokk2021 where

import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Utils

data User = User
  { felhasznaloNev :: String
  , jelszoHash :: String
  , tojasNev :: String
  }

userJsonEncoder :: User -> Json
userJsonEncoder user = JsonObject
  [ ("nev", JsonString $ felhasznaloNev user)
  , ("fhs", JsonString $ jelszoHash user)
  , ("tnv", JsonString $ tojasNev user)
  ]

userDecoder :: Decoder User
userDecoder =
    User <$> Decoder.field "nev" Decoder.string
         <*> Decoder.field "fhs" Decoder.string
         <*> Decoder.field "tnv" Decoder.string

instance DBFormat User where
  encode = Text.pack . show . userJsonEncoder
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run userDecoder)
    . Json.parseJson
    . Text.unpack
