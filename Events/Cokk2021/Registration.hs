module Events.Cokk2021.Registration where

import           Events.Cokk2021.User (User(User))
import qualified Events.Cokk2021.User as User
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Events.Cokk2021.Skills as Skills
import qualified Events.Cokk2021.Item as Item


data Registration = Registration
  { username :: String
  , password :: String
  , eggName :: String
  }

decode :: Decoder Registration
decode =
  Registration <$> Decoder.field "username" Decoder.string
               <*> Decoder.field "password" Decoder.string
               <*> Decoder.field "eggname" Decoder.string

toUser :: Registration -> User
toUser reg = User
  { User.username = username reg
  , User.passwordHash = password reg
  , User.eggname = eggName reg
  , User.perfume = 5
  , User.base = Item.initialBase
  , User.skills = Skills.initial
  , User.items = []
  }
