module Types.Password (Password(..), decoder) where


import Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder

newtype Password = Password String

decoder :: Decoder Password
decoder = Password <$> Decoder.field "password" Decoder.string
