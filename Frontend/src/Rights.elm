module Rights exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Html exposing (text)

type Model = Model

type Msg = Msg

init : (Model, Cmd Msg)
init = (Model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

view : Model -> Document Msg
view _ =
  { title = "Films"
  , body =
    [ CDN.stylesheet
    , [ [ [text "oi"] |> Grid.col []] |> Grid.row [] ] |> Grid.container []
    ]
  }
