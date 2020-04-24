module Articles exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, text, h1, h2, p, a)
import Html.Attributes exposing (href)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

type Model = NoModel
type Msg = NoMsg
  
init : (Model, Cmd Msg)
init = (NoModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Document Msg
view model =
  { title = "Articles"
  , body = 
    [ [ CDN.stylesheet
      , [ [ h1 [] [text "Articles!"]
          , p [] [ h2 [] [text "A taxonomy and platform for anomaly detection"]
                 , text "Here is the link: "
                 , a [href "/res/anomaly_detection_taxonomy.pdf"] [text "PDF"]
                 , text "."
                 ]
          , p [] [ h2 [] [text "Platform for Anomaly Detection in Time-Series"]
                 , text "Here is the link: "
                 , a [href "/res/anomaly_detection_platform.pdf"] [text "PDF"]
                 , text "."
                 ]
          , p [] [ h2 [] [text "Evaluation metrics for anomaly detection algorithms in time-series"]
                 , text "Here is the link: "
                 , a [href "/res/anomaly_detection_metrics.pdf"] [text "PDF"]
                 , text "."
                 ]
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }
