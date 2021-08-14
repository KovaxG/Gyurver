{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.ChangeEggnameRequest where

import           Data.Text (Text)

import           Events.Cokk2021.Login (Login (..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

data ChangeEggnameRequest = CENR
  { username :: Text
  , password :: Text
  , newEggname :: Text
  }

decode :: Decoder ChangeEggnameRequest
decode =
  CENR <$> Decoder.field "username" Decoder.string
       <*> Decoder.field "password" Decoder.string
       <*> Decoder.field "newEggname" Decoder.string

toLogin :: ChangeEggnameRequest -> Login
toLogin CENR {username, password} = Login username password
