module Films exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html, div, text)
import Time exposing (Month(..))
import Http
import List.Extra as List
import Random

import Types.Language exposing (Language(..))
import Endpoints
import Util
import Settings


type alias Film =
  { title : String
  , watched : Bool
  }

type alias OkState =
  { films : List Film
  , random : Maybe Film
  }

type Model
  = ShowMessage String
  | Ok OkState

decodeFilm : Decoder Film
decodeFilm =
  Decode.map2
    Film
    (Decode.field "title" Decode.string)
    (Decode.field "watched" Decode.bool)

type Msg
  = Populate (List Film)
  | PopulateError String
  | ChooseRandomFilm
  | RandomFilm Int

init : (Model, Cmd Msg)
init =
  ( ShowMessage "Waiting for server to send me some films..."
  , Http.get
    { url = Settings.path ++ Endpoints.filmItemsJson
    , expect =
      Http.expectJson
        (Util.processMessage Populate PopulateError)
        (Decode.list decodeFilm)
    }
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  PopulateError err -> (ShowMessage err, Cmd.none)
  Populate films -> (Ok { films = films, random = Nothing }, Cmd.none)
  ChooseRandomFilm ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (model, Random.generate RandomFilm <| Random.int 0 (List.length state.films))
  RandomFilm index ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | random = getRandomUnwatched index state.films }, Cmd.none)

getRandomUnwatched : Int -> List Film -> Maybe Film
getRandomUnwatched index films =
  let unwatched = List.filter (not << .watched) films
      wrapIndex = modBy (List.length unwatched) index
  in List.getAt wrapIndex unwatched

view : Model -> Document Msg
view state =
  { title = "Films"
  , body =
    [ CDN.stylesheet
    , [ showModel state |> Grid.row [] ] |> Grid.container []
    ]
  }

showModel : Model -> List (Grid.Column Msg)
showModel model =
  case model of
    ShowMessage msg ->
      [ Grid.col [] [ text msg ] ]
    Ok state ->
      [ showFilmPanel state
      , showFilms state.films
      ]

showFilms : List Film -> Grid.Column Msg
showFilms films = [ div [] (List.indexedMap showFilm films) ] |> Grid.col []

showFilm : Int -> Film -> Html Msg
showFilm index film = div [] (text (String.fromInt (index + 1) ++ ". ") :: text film.title :: if film.watched then [text " ✔️"] else [])
showFilmPanel : OkState -> Grid.Column Msg
showFilmPanel state =
  [ Button.button
      [ Button.outlinePrimary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick ChooseRandomFilm
      ] [text "Random Film"]
  , state.random |> Maybe.map (text << .title) |> Maybe.withDefault (text "")
  ] |> Grid.col []
