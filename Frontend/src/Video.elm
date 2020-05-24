module Video exposing (Video, decode, encode)

import Date as Date exposing (Date)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)

type alias Video =
  { url : String
  , title : String
  , author : String
  , date : Date
  , comment : String
  , watchDate : Maybe Date
  , tags : List String
  }
  
decode : Decoder Video
decode =
  Decode.map7
    Video
    (Decode.field "url" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "author" Decode.string)
    (Decode.field "date" decodeDate)
    (Decode.field "comment" Decode.string)
    (Decode.field "watchDate" (Decode.maybe decodeDate))
    (Decode.field "tags" (Decode.list Decode.string))
    
decodeDate : Decoder Date
decodeDate =
  Decode.map3
    (\y m d -> 
      Date.fromCalendarDate y 
                            (Date.numberToMonth m) 
                            d
    )
    (Decode.field "year" Decode.int)
    (Decode.field "month" Decode.int)
    (Decode.field "day" Decode.int)
    
encode : Video -> Value
encode var = 
  Encode.object
    [ ("url", Encode.string var.url)
    , ("title", Encode.string var.title)
    , ("author", Encode.string var.author)
    , ("date", encodeDate var.date)
    , ("comment", Encode.string var.comment)
    , ("watchDate", Maybe.withDefault Encode.null <| Maybe.map encodeDate var.watchDate)
    , ("tags", Encode.list Encode.string var.tags)
    ]

encodeDate : Date -> Value
encodeDate date =
  Encode.object
    [ ("year", Encode.int <| Date.year date)
    , ("month", Encode.int <| Date.monthNumber date)
    , ("day", Encode.int <| Date.day date)
    ]
