{-# LANGUAGE NamedFieldPuns #-}
module Settings(Settings(..), parse, defaultSettings) where

import qualified Data.Bifunctor as Bifunctor
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec

import qualified Utils

data Settings = Settings
  { hostAddress :: String
  , password :: String
  } deriving (Show)

defaultSettings = Settings
  { hostAddress = "localhost"
  , password = "nincs jelszo"
  }

parse :: String -> Either String Settings
parse = Bifunctor.first show . Parsec.parse settings "Parsing Settings"
  where
    settings = do
      Parsec.string "host_address"
      Parsec.spaces
      hostAddress <- Parsec.many1 (Parsec.alphaNum <|> Parsec.char '.')
      Parsec.newline
      Parsec.string "password"
      Parsec.spaces
      password <- Parsec.many1 (Parsec.noneOf "\n")
      return Settings { hostAddress, password }
