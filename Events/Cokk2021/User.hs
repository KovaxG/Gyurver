module Events.Cokk2021.User where

import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import qualified Component.Database as DB
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Utils

data User = User
  { username :: String
  , passwordHash :: String
  , eggname :: String
  , perfume :: Int
  , image :: String
  }

encode :: User -> Json
encode user = JsonObject
  [ ("username", JsonString $ username user)
  , ("password", JsonString $ passwordHash user)
  , ("eggname", JsonString $ eggname user)
  , ("perfume", JsonNumber $ fromIntegral $ perfume user)
  , ("image", JsonString $ image user)
  ]

decode :: Decoder User
decode =
  User <$> Decoder.field "username" Decoder.string
       <*> Decoder.field "password" Decoder.string
       <*> Decoder.field "eggname" Decoder.string
       <*> Decoder.field "perfume" Decoder.int
       <*> Decoder.field "image" Decoder.string

instance DBFormat User where
  encode = Text.pack . show . Events.Cokk2021.User.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.User.decode)
    . Json.parseJson
    . Text.unpack

addPerfume :: Int -> User -> User
addPerfume p u = u { perfume = perfume u + p }

toListItemJson :: Bool -> User -> Json
toListItemJson b u = JsonObject
  [ ("username", JsonString $ username u)
  , ("eggname", JsonString $ eggname u)
  , ("image", JsonString $ image u)
  , ("waterable", JsonBool b)
  ]
