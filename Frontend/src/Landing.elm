module Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Html exposing (text, h1, p, a)
import Html.Attributes exposing (href)

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
      , h1 [] [text "Welcome to Gyurver!"]
      , p [] [ text "Welcome to my site!" ]
      , p [] [ text "🗎 Check out my "
             , a [href "/cv"] [text "CV"]
             , text "."
             ]
      , p [] [ text "🗎 I also write some "
             , a [href "/articles"] [text "articles"]
             , text ", check them out."
             ]
      , p [] [ text "📼 I also have a list of videos I like, feel free to "
             , a [href "/vids"] [text "check them out"]
             , text "."
             ]
      , p [] [ a [href "/cokk"] [text "🥚 Cokkoleses verseny 2020"]]
      ] |> Grid.container []
    ]
  }
