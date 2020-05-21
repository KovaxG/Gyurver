module Vids exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, text, h1, h2, h3, p, a, br, strong, div, iframe)
import Html.Attributes exposing (src)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing as Spacing
import Date exposing (Date)
import Time exposing (Month(..))
type alias Model = List Video


type alias Video =
  { link : String
  , title : String
  , channel : String
  , date : Date
  , comment : String
  , watchDate : Date
  , tags : List String
  }

type alias Msg = ()

init : (Model, Cmd Msg)
init = (testVids, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

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
    [ Grid.col [] [iframe [src vid.link] []]
    , Grid.col []
      [ strong [] [text "Channel "]
      , text vid.channel
      , br [] []
      , strong [] [text "Date "]
      , text (Date.toIsoString vid.date)
      , br [] []
      , strong [] [text "Watch Date "]
      , text (Date.toIsoString vid.watchDate)
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

testVids : List Video
testVids =
  [ { link = "https://www.youtube.com/embed/8aGhZQkoFbQ"
    , title = "What the heck is the event loop anyway? | Philip Roberts | JSConf EU"
    , channel = "JSConf"
    , date = Date.fromCalendarDate 2014 Oct 9
    , comment = "Gyakran hallottam az event loopról, s kb tudom hogy mi az, de sosem néztem utána. Szerintem ebben a videóban egész jól elmagyarázzák, s van jó animáció is."
    , watchDate = Date.fromCalendarDate 2020 Apr 18
    , tags = ["JavaScript","Event Loop"]
    }
  , { link = "https://www.youtube.com/embed/RFrKffrKCeU"
    , title = "I used Elm in production and it cost me my job Annaia Berry"
    , channel = "JetBrainsTV"
    , date = Date.fromCalendarDate 2018 Oct 15
    , comment = "..."
    , watchDate = Date.fromCalendarDate 2020 Apr 24
    , tags = ["Database"]
    }
  ]