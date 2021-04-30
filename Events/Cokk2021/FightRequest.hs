module Events.Cokk2021.FightRequest where

import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Component.Json (Json(..))
import           Gyurver.Logger (Logger)
import           Events.Cokk2021.User (User)
import qualified Events.Cokk2021.User as User
import qualified Events.Cokk2021.Bajnoksag as Bajnoksag

data FightRequest = FightRequest
  { source :: String
  , target :: String
  , sourcePass :: String
  }

decode :: Decoder FightRequest
decode =
  FightRequest <$> Decoder.field "username" Decoder.string
               <*> Decoder.field "target" Decoder.string
               <*> Decoder.field "password" Decoder.string

runFight :: User -> User -> Logger -> IO [Json]
runFight a b logger =
  fmap Bajnoksag.encodeLog . Bajnoksag.logs
    <$> Bajnoksag.fight (Bajnoksag.mkTojas (User.eggname a) (User.skills a)
                        , Bajnoksag.mkTojas (User.eggname b) (User.skills b)
                        ) logger
