module Blog exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Json.Decode as Decode exposing (Decoder)
import Date exposing (fromCalendarDate, toIsoString)
import Html exposing (Html, div, br, h3, a, strong, text)
import Html.Attributes exposing (href)
import Time exposing (Month(..))
import Http

import Types.Date as Date
import Types.Language as Language exposing (Language(..))
import Types.Date exposing (Date)
import Endpoints
import Util
import Settings

type alias Model = {}

type Msg = NoMsg

init : (Model, Cmd Msg)
init = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Document Msg
view state =
  { title = "Articles"
  , body =
    [ CDN.stylesheet
    , [ [ [ text "This WILL be a blogpost!"
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }
