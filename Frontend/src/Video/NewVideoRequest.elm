module Video.NewVideoRequest exposing (..)

import Date exposing (Date)
import Json.Encode as Encode exposing (Value)
import Types.Date as Date

type alias NewVideoRequest =
  { url : String
  , title : String
  , author : String
  , date : Date
  , comment : String
  , watchDate : Maybe Date
  , tags : List String
  , password : String
  }

encode : NewVideoRequest -> Value
encode var =
  Encode.object
    [ ("url", Encode.string var.url)
    , ("title", Encode.string var.title)
    , ("author", Encode.string var.author)
    , ("date", Date.encode var.date)
    , ("comment", Encode.string var.comment)
    , ("watchDate", Maybe.withDefault Encode.null <| Maybe.map Date.encode var.watchDate)
    , ("tags", Encode.list Encode.string var.tags)
    , ("password", Encode.string var.password)
    ]
