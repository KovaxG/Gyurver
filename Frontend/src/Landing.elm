module Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Html exposing (text, p, a)
import Html.Attributes exposing (href)
import Http

import Endpoints
import Settings

type alias Model = Maybe Int
type Msg
  = PopulateTodayHits Int
  | NoMsg

init : (Model, Cmd Msg)
init =
  ( Nothing
  , Http.get
    { url = Settings.path ++ Endpoints.pageHitsToday
    , expect = Http.expectString (\response -> case response of
        Ok numberString ->
          numberString
          |> String.toInt
          |> Maybe.map PopulateTodayHits
          |> Maybe.withDefault NoMsg
        Err _ -> NoMsg
      )
    }
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  PopulateTodayHits hits -> (Just hits, Cmd.none)
  NoMsg -> (model, Cmd.none)

view : Model -> Document Msg
view hits =
  let
    hitsText = case hits of
      Just n -> "I processed " ++ String.fromInt n ++ " requests today!"
      Nothing -> "For some reason the server doesn't respond with the number of hits for today. Maybe something is up with the DB?"
  in
  { title = "Welcome"
  , body =
    [ [ CDN.stylesheet
      , p [] [ text <| "Welcome to my site!" ]
      , p [] [ text "📑 Check out my "
             , a [href Endpoints.cv] [text "CV"]
             , text "."
             ]
      , p [] [ text hitsText]
      ] |> Grid.container []
    ]
  }
