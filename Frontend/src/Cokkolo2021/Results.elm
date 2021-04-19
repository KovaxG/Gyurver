module Cokkolo2021.Results exposing (..)

import Html exposing (Html, div)
import Browser exposing (Document)
import Bootstrap.CDN as CDN

type alias Model = {}
type Msg = NoMsg

init : (Model, Cmd Msg)
init = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ m = (m, Cmd.none)

view : Model -> Document Msg
view _ =
  { title = "Cokkolo 2021 Results"
  , body = [CDN.stylesheet]
  }
