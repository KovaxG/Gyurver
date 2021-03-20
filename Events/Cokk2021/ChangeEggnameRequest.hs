{-# LANGUAGE NamedFieldPuns #-}
module Events.Cokk2021.ChangeEggnameRequest where

import           Events.Cokk2021.Login (Login (..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

data ChangeEggnameRequest = CENR
  { username :: String
  , password :: String
  , newEggname :: String
  }

decode :: Decoder ChangeEggnameRequest
decode =
  CENR <$> Decoder.field "username" Decoder.string
       <*> Decoder.field "password" Decoder.string
       <*> Decoder.field "newEggname" Decoder.string

toLogin :: ChangeEggnameRequest -> Login
toLogin CENR {username, password} = Login username password
