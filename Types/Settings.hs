{-# LANGUAGE NamedFieldPuns #-}
module Types.Settings(Settings(..), parse, defaultSettings) where

import qualified Data.Bifunctor as Bifunctor
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec

import qualified Utils
import Gyurver.Server

data Settings = Settings
  { hostAddress :: IP
  , port :: Port
  , password :: String
  } deriving (Show)

defaultSettings = Settings
  { hostAddress = IP "localhost"
  , port = Port 8080
  , password = "nincs jelszo"
  }

parse :: String -> Either String Settings
parse = Bifunctor.first show . Parsec.parse settings "Parsing Settings"
  where
    settings = do
      Parsec.string "host_address"
      Parsec.spaces
      hostAddress <- IP <$> Parsec.many1 (Parsec.alphaNum <|> Parsec.char '.')
      Parsec.newline
      Parsec.string "port"
      Parsec.spaces
      port <- Port . read <$> Parsec.many1 Parsec.digit
      Parsec.newline
      Parsec.string "password"
      Parsec.spaces
      password <- Parsec.many1 Parsec.alphaNum
      return Settings { hostAddress, port, password }
