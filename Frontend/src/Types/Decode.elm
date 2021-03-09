module Types.Decode exposing (..)

import Json.Decode as Decode exposing (Decoder)

andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap da =  Decode.andThen (\f -> Decode.map f da)
