module Events.Cokk2021.FightRequest where

import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Component.Json (Json(..))
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

runFight :: User -> User -> IO [Json]
runFight a b =
  fmap Bajnoksag.encodeLog . Bajnoksag.logs
    <$> Bajnoksag.fight (Bajnoksag.mkTojas (User.username a) (User.skills a)
                        , Bajnoksag.mkTojas (User.username b) (User.skills b)
                        )
