module Types.Video exposing (Video, decode, encode)

import Date as Date exposing (Date)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)

import Types.Date as Date

type alias Video =
  { nr : Int
  , url : String
  , title : String
  , author : String
  , date : Date
  , comment : String
  , watchDate : Maybe Date
  , tags : List String
  }

decode : Decoder Video
decode =
  Decode.map8
    Video
    (Decode.field "nr" Decode.int)
    (Decode.field "url" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "author" Decode.string)
    (Decode.field "date" Date.decode)
    (Decode.field "comment" Decode.string)
    (Decode.field "watchDate" (Decode.maybe Date.decode))
    (Decode.field "tags" (Decode.list Decode.string))

encode : Video -> Value
encode video =
  Encode.object
    [ ("nr", Encode.int video.nr)
    , ("url", Encode.string video.url)
    , ("title", Encode.string video.title)
    , ("author", Encode.string video.author)
    , ("date", Date.encode video.date)
    , ("comment", Encode.string video.comment)
    , ("watchDate", Maybe.withDefault Encode.null <| Maybe.map Date.encode video.watchDate)
    , ("tags", Encode.list Encode.string video.tags)
    ]
