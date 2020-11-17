module Types.Date exposing (Date, decode, encode)

import Date as ElmDate
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias Date = ElmDate.Date

decode : Decoder Date
decode =
  Decode.map3
    (\y m d ->
      ElmDate.fromCalendarDate y
                            (ElmDate.numberToMonth m)
                            d
    )
    (Decode.field "year" Decode.int)
    (Decode.field "month" Decode.int)
    (Decode.field "day" Decode.int)

encode : Date -> Value
encode date =
  Encode.object
    [ ("year", Encode.int <| ElmDate.year date)
    , ("month", Encode.int <| ElmDate.monthNumber date)
    , ("day", Encode.int <| ElmDate.day date)
    ]
