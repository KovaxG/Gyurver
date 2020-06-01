module Types.Date exposing (decode, encode)

import Date as Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

decode : Decoder Date
decode =
  Decode.map3
    (\y m d ->
      Date.fromCalendarDate y
                            (Date.numberToMonth m)
                            d
    )
    (Decode.field "year" Decode.int)
    (Decode.field "month" Decode.int)
    (Decode.field "day" Decode.int)

encode : Date -> Value
encode date =
  Encode.object
    [ ("year", Encode.int <| Date.year date)
    , ("month", Encode.int <| Date.monthNumber date)
    , ("day", Encode.int <| Date.day date)
    ]
