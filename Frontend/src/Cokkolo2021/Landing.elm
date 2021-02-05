module Cokkolo2021.Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h3, p, ol, li, br, a)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text

type alias Model = Int
type Msg = NoMsg

init : (Model, Cmd Msg)
init = (0, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ _ = (0, Cmd.none)

view : Model -> Document Msg
view _ =
  { title = "Cokkolo 2021"
  , body =
    [ [ CDN.stylesheet
      , [ [ h1 [] [text "2021 Húsvéti játékok"]
          , text "Itt kene legyen a leiras"
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }
