{-# LANGUAGE OverloadedStrings #-}

module Types.Password (Password(..), decoder, valid, invalid) where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

newtype Password = Password Text deriving (Eq)

instance Show Password where
    show (Password t) = replicate (Text.length t) '*'

valid :: Password -> Text -> Bool
valid (Password p) t = p == t

invalid :: Password -> Text -> Bool
invalid p = not . valid p

decoder :: Decoder Password
decoder = Password <$> Decoder.field "password" Decoder.string
