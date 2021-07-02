module Blog exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser exposing (Document)
import Html exposing (text)

type alias Model = ()
type Msg = NoMsg

init : (Model, Cmd Msg)
init = ((), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

view : Model -> Document Msg
view _ =
  { title = "Articles"
  , body =
    [ CDN.stylesheet
    , Grid.container [] [Grid.row [] [Grid.col [] [text "Under development..."]]]
    ]
  }
