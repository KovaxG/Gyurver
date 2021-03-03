module Types.DateTime exposing (DateTime, encode, decode)

import Date as ElmDate
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias DateTime =
  { year : Int
  , month : Int
  , day : Int
  , hour : Int
  , minutes : Int
  , seconds : Float
  }

decode : Decoder DateTime
decode =
  Decode.map6
    DateTime
    (Decode.field "year" Decode.int)
    (Decode.field "month" Decode.int)
    (Decode.field "day" Decode.int)
    (Decode.field "hours" Decode.int)
    (Decode.field "minutes" Decode.int)
    (Decode.field "seconds" Decode.float)

encode : DateTime -> Value
encode dt =
  Encode.object
    [ ("year", Encode.int dt.year)
    , ("month", Encode.int dt.month)
    , ("day", Encode.int dt.day)
    , ("hours", Encode.int dt.hour)
    , ("minutes", Encode.int dt.minutes)
    , ("seconds", Encode.float dt.seconds)
    ]
