module Events.Cokk2021.Login where

import           Events.Cokk2021.User (User(User))
import qualified Events.Cokk2021.User as User
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

data Login = Login
  { user :: String
  , pass :: String
  }

decode :: Decoder Login
decode =
  Login <$> Decoder.field "user" Decoder.string
        <*> Decoder.field "pass" Decoder.string

matchesLogin :: Login -> User -> Bool
matchesLogin l u = User.username u == user l && User.passwordHash u == pass l
