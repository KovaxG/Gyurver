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
  , kolni :: Int
  , kep :: String
  }

data Registration = Registration
  { username :: String
  , password :: String
  , eggName :: String
  }

data WaterRequest = WaterRequest
  { source :: String
  , target :: String
  , sourcePass :: String
  }

waterRequestDecoder :: Decoder WaterRequest
waterRequestDecoder =
  WaterRequest <$> Decoder.field "username" Decoder.string
               <*> Decoder.field "target" Decoder.string
               <*> Decoder.field "password" Decoder.string

userRegistrationDecoder :: Decoder Registration
userRegistrationDecoder =
  Registration <$> Decoder.field "username" Decoder.string
               <*> Decoder.field "password" Decoder.string
               <*> Decoder.field "eggname" Decoder.string

registrationToUser :: Registration -> User
registrationToUser reg = User
  { felhasznaloNev = username reg
  , jelszoHash = password reg
  , tojasNev = eggName reg
  , kolni = 0
  , kep = "pucer"
  }

userJsonEncoder :: User -> Json
userJsonEncoder user = JsonObject
  [ ("username", JsonString $ felhasznaloNev user)
  , ("password", JsonString $ jelszoHash user)
  , ("eggname", JsonString $ tojasNev user)
  , ("perfume", JsonNumber $ fromIntegral $ kolni user)
  , ("image", JsonString $ kep user)
  ]

userJsonDecoder :: Decoder User
userJsonDecoder =
  User <$> Decoder.field "username" Decoder.string
       <*> Decoder.field "password" Decoder.string
       <*> Decoder.field "eggname" Decoder.string
       <*> Decoder.field "perfume" Decoder.int
       <*> Decoder.field "image" Decoder.string

userToListItemJson :: User -> Json
userToListItemJson u = JsonObject
  [ ("username", JsonString $ felhasznaloNev u)
  , ("eggname", JsonString $ tojasNev u)
  , ("image", JsonString $ kep u)
  ]

instance DBFormat User where
  encode = Text.pack . show . userJsonEncoder
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run userJsonDecoder)
    . Json.parseJson
    . Text.unpack

data Login = Login
  { user :: String
  , pass :: String
  }

loginDecoder :: Decoder Login
loginDecoder =
  Login <$> Decoder.field "user" Decoder.string
        <*> Decoder.field "pass" Decoder.string
