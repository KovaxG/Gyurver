module Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Html exposing (text, h1, p, a)
import Html.Attributes exposing (href)

import Endpoints
import Settings

type alias Model = ()
type alias Msg = ()

init : (Model, Cmd Msg)
init = ((), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ _ = ((), Cmd.none)

view : Model -> Document Msg
view _ =
  { title = "Welcome"
  , body =
    [ [ CDN.stylesheet
      , p [] [ text <| "Welcome to my site!" ]
      , p [] [ text "📑 Check out my "
             , a [href Endpoints.cv] [text "CV"]
             , text "."
             ]
      ] |> Grid.container []
    ]
  }
