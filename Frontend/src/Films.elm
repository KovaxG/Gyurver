module Films exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.Input as Input
import Browser exposing (Document)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html, div, text, br)
import Time exposing (Month(..))
import Http
import List.Extra as List
import Random

import Types.Language exposing (Language(..))
import Endpoints
import Util
import Settings

type alias Password = String

type alias Film =
  { title : String
  , watched : Bool
  }

type alias OkState =
  { films : List Film
  , random : Maybe Film
  , newFilm : Maybe String
  , secret : String
  , errorMsg : String
  , editMode : Bool
  , deleteMode : Bool
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
  | ShowInputField Bool
  | NewFilmChanged String
  | SecretChanged String
  | AddNewFilm String Password
  | AddNewFilmSuccess String
  | ChangeEditMode
  | DeleteMovie String Password
  | DeleteMovieSuccess String
  | FilmWatched String Password
  | FilmWatchedSuccess String
  | ToggleDeleteMode
  | Failure String

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
  Populate films -> (Ok { films = films, random = Nothing, newFilm = Nothing, secret = "", errorMsg = "", editMode = False, deleteMode = False }, Cmd.none)
  ChooseRandomFilm ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (model, Random.generate RandomFilm <| Random.int 0 (List.length state.films))
  RandomFilm index ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | random = getRandomUnwatched index state.films }, Cmd.none)
  ShowInputField b ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state ->
        if b
        then (Ok { state | newFilm = Just "", errorMsg = "" }, Cmd.none)
        else (Ok { state | newFilm = Nothing, errorMsg = "" }, Cmd.none)
  NewFilmChanged ft ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | newFilm = Just ft }, Cmd.none)
  SecretChanged ss ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | secret = ss }, Cmd.none)
  AddNewFilm title secret ->
    ( model
    , Http.request
      { method = "POST"
      , headers = [ Http.header "Gyursecret" secret ]
      , url = Settings.path ++ Endpoints.filmItemsJson
      , body = Http.stringBody "text" title
      , expect = Http.expectWhatever <| Util.processMessage (always <| AddNewFilmSuccess title) Failure
      , timeout = Nothing
      , tracker = Nothing
      }
    )
  AddNewFilmSuccess title ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | newFilm = Just "", errorMsg = "", films = state.films ++ [Film title False] }, Cmd.none)
  Failure err ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | errorMsg = err }, Cmd.none)
  ChangeEditMode ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | editMode = not state.editMode, secret = "", errorMsg = "" }, Cmd.none)
  DeleteMovie title secret ->
    ( model
    , Http.request
      { method = "DELETE"
      , headers = [ Http.header "Gyursecret" secret ]
      , url = Settings.path ++ Endpoints.filmItemsJson
      , body = Http.stringBody "text" title
      , expect = Http.expectWhatever <| Util.processMessage (always <| DeleteMovieSuccess title) Failure
      , timeout = Nothing
      , tracker = Nothing
      }
    )
  DeleteMovieSuccess title ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | films = state.films |> List.filter (\f -> f.title /= title), errorMsg = "" }, Cmd.none)
  FilmWatched title secret ->
    ( model
    , Http.request
      { method = "PUT"
      , headers = [ Http.header "Gyursecret" secret ]
      , url = Settings.path ++ Endpoints.filmItemsJson
      , body = Http.stringBody "text" title
      , expect = Http.expectWhatever <| Util.processMessage (always <| FilmWatchedSuccess title) Failure
      , timeout = Nothing
      , tracker = Nothing
      }
    )
  FilmWatchedSuccess title ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | errorMsg = "", films = state.films |> List.map (\f -> if f.title == title then { f | watched = not f.watched} else f) }, Cmd.none)
  ToggleDeleteMode ->
    case model of
      ShowMessage _ -> (model, Cmd.none)
      Ok state -> (Ok { state | deleteMode = not state.deleteMode }, Cmd.none)

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
      , showFilms state.secret state.deleteMode state.editMode state.films
      ]

showFilms : Password -> Bool -> Bool -> List Film -> Grid.Column Msg
showFilms secret deleteMode editMode films = [ div [] (List.indexedMap (showFilm secret deleteMode editMode) films) ] |> Grid.col []

showFilm : Password -> Bool -> Bool -> Int -> Film -> Html Msg
showFilm secret deleteMode editMode index film =
  let deleteButton =
        Button.button
          [ Button.danger
          , Button.attrs [ Spacing.m1 ]
          , Button.onClick <| DeleteMovie film.title secret
          ] [text "🗑️"]
      watchedButton =
        Button.button
          [ Button.info
          , Button.attrs [ Spacing.m1 ]
          , Button.onClick <| FilmWatched film.title secret
          ] [text <| if film.watched then "✖️" else "✔️"]
  in
    (  (if editMode && deleteMode then deleteButton else text "")
    :: (if editMode then watchedButton else text "")
    :: text (String.fromInt (index + 1) ++ ". ")
    :: text film.title :: if film.watched then [text " ✔️"] else []
    ) |> div []

showFilmPanel : OkState -> Grid.Column Msg
showFilmPanel state =
  [ randomFilmSection state.random
  , editPanel state
  ] |> Grid.col []

randomFilmSection : Maybe Film -> Html Msg
randomFilmSection selected =
  [ Button.button
    [ Button.outlinePrimary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick ChooseRandomFilm
    ] [text "Random Film"]
  , selected |> Maybe.map (text << .title) |> Maybe.withDefault (text "")
  ] |> div []

editPanel  : OkState -> Html Msg
editPanel state =
  [ Button.button
    [ if state.editMode then Button.outlineDark else Button.outlineInfo
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick ChangeEditMode
    ] [text <| if state.editMode then "🔙" else "✍️"]
  , div [] <| if state.editMode
    then
      [ newFilmSection state
      , text "Secret: "
      , Input.password [ Input.small, Input.value state.secret, Input.onInput SecretChanged ]
      , br [] []
      , Button.button
        [ Button.outlineDanger
        , Button.attrs [ Spacing.m1 ]
        , Button.onClick ToggleDeleteMode
        ] [text "🗑️"]
  , br [] []
      ]
    else []
  , text state.errorMsg
  ] |> div []

newFilmSection : OkState -> Html Msg
newFilmSection state =
  case state.newFilm of
    Nothing ->
      [ Button.button
        [ Button.outlineSuccess
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick (ShowInputField True)
        ] [text "➕"]
      ] |> div []
    Just newFilm ->
      [ Button.button
        [ Button.outlineDark
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick (ShowInputField False)
        ] [text "✖️"]
      , Button.button
        [ Button.outlineSuccess
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick (AddNewFilm newFilm state.secret)
        ] [text "✔️"]
      , br [] []
      , text "Film: "
      , Input.text [Input.small, Input.value newFilm, Input.onInput NewFilmChanged]
      ] |> div []
