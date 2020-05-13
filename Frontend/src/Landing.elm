module Landing exposing (Model, Msg, init, update, view)

import Browser exposing (UrlRequest, Document, application)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Html exposing (Html, text, button, h1, p, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)

type Model = Model String

type Msg = Change String

init : (Model, Cmd Msg)
init = (Model "What", Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update (Change ch) _ = (Model ch, Cmd.none)
  
view : Model -> Document Msg
view (Model str) = 
  { title = "Welcome"
  , body = 
    [ [ CDN.stylesheet
      , h1 [] [text "Welcome to Gyurver!"]
      , p [] [text "Welcome to my site!"]
      , p [] [ text "🗎 Check out my " 
             , a [href "/cv"] [text "CV"]
             , text "."
             ]
      , p [] [ text "🗎 I also write some "
             , a [href "/articles"] [text "articles"]
             , text ", check them out."
             ]
      , p [] [ a [] [text "📼 I also have a list of videos I like, feel free to check them out"]]
      , p [] [ a [href "/cokk"] [text "🥚 Cokkoleses verseny 2020"]]
      ] |> Grid.container []
    ] 
  }
