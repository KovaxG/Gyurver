module Events.Cokk2021 where

import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Types.DateTime (DateTime)
import qualified Types.DateTime as DateTime
import qualified Utils

data DashboardData = DashboardData
  { userData :: User
  , events :: [WaterLog]
  }

mkDashboardData :: User -> [WaterLog] -> DashboardData
mkDashboardData user log =
  let outgoing = filter (\l -> wlSource l == felhasznaloNev user) log
      incoming = filter (\l -> wlTarget l == felhasznaloNev user) log
  in DashboardData user (outgoing ++ incoming)

dashboardDataEncoder :: DashboardData -> Json
dashboardDataEncoder (DashboardData user events) = JsonObject
  [ ("username", JsonString $ felhasznaloNev user)
  , ("password", JsonString $ jelszoHash user)
  , ("eggname", JsonString $ tojasNev user)
  , ("perfume", JsonNumber $ fromIntegral $ kolni user)
  , ("image", JsonString $ kep user)
  , ("events", JsonArray $ waterLogToJson <$> events)
  ]

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

data WaterLog = WaterLog
  { wlSource :: String
  , wlTarget :: String
  , wlDateTime :: DateTime
  }

mkWaterLog :: String -> String -> IO WaterLog
mkWaterLog source target = WaterLog source target <$> DateTime.getCurrentDateTime

waterLogToJson :: WaterLog -> Json
waterLogToJson wl = JsonObject
  [ ("source", JsonString $ wlSource wl)
  , ("target", JsonString $ wlTarget wl)
  , ("time", DateTime.toJson $ wlDateTime wl)
  ]

waterLogDecoder :: Decoder WaterLog
waterLogDecoder =
  WaterLog <$> Decoder.field "source" Decoder.string
           <*> Decoder.field "target" Decoder.string
           <*> Decoder.field "time" DateTime.decoder

instance DBFormat WaterLog where
  encode = Text.pack . show . waterLogToJson
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run waterLogDecoder)
    . Json.parseJson
    . Text.unpack
