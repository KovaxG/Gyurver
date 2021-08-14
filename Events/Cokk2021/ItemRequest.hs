{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.ItemRequest where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Events.Cokk2021.Login (Login (..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

data ItemRequest = IR
  { username :: Text
  , password :: Text
  , index :: Int
  } deriving (Show)

decode :: Decoder ItemRequest
decode =
  IR <$> Decoder.field "username" Decoder.string
     <*> Decoder.field "password" Decoder.string
     <*> Decoder.field "index" Decoder.int

toLogin :: ItemRequest -> Login
toLogin IR {username, password} = Login username password
