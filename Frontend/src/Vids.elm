module Vids exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, text, h1, h3, br, strong, div, iframe)
import Html.Attributes exposing (src)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing as Spacing
import Date exposing (Date)
import Time exposing (Month(..))
import Http exposing (Error)
import Settings
import Json.Decode as Decode exposing (Decoder)

import Types.Video as Video exposing (Video)

type alias Model = List Video

type Msg = SetVideos (List Video)

init : (Model, Cmd Msg)
init =
  ( [] -- TODO maybe add a loading state?
  , Http.get
    { url = Settings.path ++ "/api/vids"
    , expect = Http.expectJson toMessage (Decode.list Video.decode)
    }
  )

toMessage : Result Error (List Video) -> Msg
toMessage result = case result of
  Ok vids -> SetVideos vids
  Err _ -> SetVideos []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  SetVideos vids -> (vids, Cmd.none)

view : Model -> Document Msg
view videos =
  { title = "Videos"
  , body =
    [ CDN.stylesheet
    , Grid.container []
      (h1 [] [text "Videos"] :: List.map videoToHtml videos)
    ]
  }

videoToHtml : Video -> Html Msg
videoToHtml vid =
  [ Grid.row [] [Grid.col [] [h3 [] [text vid.title]]]
  , Grid.row []
    [ Grid.col [] [iframe [src vid.url] []]
    , Grid.col []
      [ strong [] [text "Channel "]
      , text vid.author
      , br [] []
      , strong [] [text "Date "]
      , text (Date.toIsoString vid.date)
      , br [] []
      , strong [] [text "Watch Date "]
      , vid.watchDate
        |> Maybe.map Date.toIsoString
        |> Maybe.withDefault "¯\\_(ツ)_/¯"
        |> text
      , br [] []
      , div []
        ( strong [] [text "Tags "]
          :: List.map (\tag -> Badge.badgePrimary [Spacing.ml1] [text tag]) vid.tags
        )
      ]
    ]
  , Grid.row []
    [ Grid.col []
      [ strong [] [text "Comment "]
      , text vid.comment
      ]
    ]
  , br [] []
  ] |> div []
