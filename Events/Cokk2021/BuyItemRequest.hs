{-# LANGUAGE NamedFieldPuns #-}
module Events.Cokk2021.BuyItemRequest where

import           Events.Cokk2021.Login (Login (..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

data BuyItemRequest = BIR
  { username :: String
  , password :: String
  , index :: Int
  } deriving (Show)

decode :: Decoder BuyItemRequest
decode =
  BIR <$> Decoder.field "username" Decoder.string
      <*> Decoder.field "password" Decoder.string
      <*> Decoder.field "index" Decoder.int

toLogin :: BuyItemRequest -> Login
toLogin BIR {username, password} = Login username password
