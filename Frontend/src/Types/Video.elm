module Types.Video exposing (Video, decode, encode)

import Date as Date exposing (Date)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)

import Types.Date as Date

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
    (Decode.field "date" Date.decode)
    (Decode.field "comment" Decode.string)
    (Decode.field "watchDate" (Decode.maybe Date.decode))
    (Decode.field "tags" (Decode.list Decode.string))

encode : Video -> Value
encode var =
  Encode.object
    [ ("url", Encode.string var.url)
    , ("title", Encode.string var.title)
    , ("author", Encode.string var.author)
    , ("date", Date.encode var.date)
    , ("comment", Encode.string var.comment)
    , ("watchDate", Maybe.withDefault Encode.null <| Maybe.map Date.encode var.watchDate)
    , ("tags", Encode.list Encode.string var.tags)
    ]
