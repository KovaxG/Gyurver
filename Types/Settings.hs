{-# LANGUAGE NamedFieldPuns #-}
module Types.Settings(Settings(..), parse, defaultSettings) where

import qualified Data.Bifunctor as Bifunctor
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec

import Gyurver.Server
import Types.Common (Mode(..))

data Settings = Settings
  { hostAddress :: IP
  , port :: Port
  , password :: String
  , mode :: Mode
  } deriving (Show)

defaultSettings = Settings
  { hostAddress = IP "localhost"
  , port = Port 8080
  , password = "nincs jelszo"
  , mode = Dev
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
      Parsec.newline
      Parsec.string "mode"
      Parsec.spaces
      mode <- mode
      return Settings { hostAddress, port, password, mode }

    mode = Parsec.try dev <|> Parsec.try prod
    dev = Parsec.string "dev" >> return Dev
    prod = Parsec.string "prod" >> return Prod