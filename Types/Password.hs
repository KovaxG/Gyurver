{-# LANGUAGE OverloadedStrings #-}

module Types.Password (Password(..), decoder) where

import Data.Text (Text)

import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

newtype Password = Password Text

decoder :: Decoder Password
decoder = Password <$> Decoder.field "password" Decoder.string
