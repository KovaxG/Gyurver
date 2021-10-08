module Films exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.Input as Input
import Browser exposing (Document)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html, div, text, br, span)
import Html.Attributes exposing (style)
import Time exposing (Month(..))
import Http
import List.Extra as List
import Random

import Types.Date as Date exposing (Date)
import Types.Language exposing (Language(..))
import Endpoints
import Util
import Settings

type alias Password = String

type alias Film =
  { title : String
  , watched : Bool
  , added : Result String Date
  , lastDiff : Maybe Date
  }

type alias Model =
  { films : List Film
  , random : Maybe Film
  , newFilm : Maybe String
  , secret : String
  , errorMsg : String
  , editMode : Bool
  , deleteMode : Bool
  , dateView : Bool
  }

initialModel : Model
initialModel =
  { films = []
  , random = Nothing
  , newFilm = Nothing
  , secret = ""
  , errorMsg = ""
  , editMode = False
  , deleteMode = False
  , dateView = False
  }

decodeFilm : Decoder Film
decodeFilm =
  Decode.map4
    Film
    (Decode.field "title" Decode.string)
    (Decode.field "watched" Decode.bool)
    (Decode.field "added" <| Decode.map Ok Date.decode)
    (Decode.field "lastModified" <| Decode.maybe Date.decode)

type Msg
  = Populate (List Film)
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
  | ToggleDateView

init : (Model, Cmd Msg)
init =
  ( { initialModel | errorMsg = "Waiting for server to send me some films..."}
  , Http.get
    { url = Settings.path ++ Endpoints.filmItemsJson
    , expect =
      Http.expectJson
        (Util.processMessage Populate Failure)
        (Decode.list decodeFilm)
    }
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Populate films -> ({ initialModel | films = films }, Cmd.none)
  ChooseRandomFilm -> (model, Random.generate RandomFilm <| Random.int 0 (List.length model.films))
  RandomFilm index -> ({ model | random = getRandomUnwatched index model.films }, Cmd.none)
  ShowInputField b ->
    if b
    then ({ model | newFilm = Just "", errorMsg = "" }, Cmd.none)
    else ({ model | newFilm = Nothing, errorMsg = "" }, Cmd.none)
  NewFilmChanged ft -> ({ model | newFilm = Just ft }, Cmd.none)
  SecretChanged ss -> ({ model | secret = ss }, Cmd.none)
  AddNewFilm title secret -> (model, request "POST" secret title AddNewFilmSuccess)
  AddNewFilmSuccess title ->
    ({ model | newFilm = Just "", errorMsg = "", films = model.films ++ [Film title False (Err "just now") Nothing] }, Cmd.none)
  Failure err -> ({ model | errorMsg = err }, Cmd.none)
  ChangeEditMode -> ({ model | editMode = not model.editMode, secret = "", errorMsg = "" }, Cmd.none)
  DeleteMovie title secret -> (model, request "DELETE" secret title DeleteMovieSuccess)
  DeleteMovieSuccess title -> ({ model | films = model.films |> List.filter (\f -> f.title /= title), errorMsg = "" }, Cmd.none)
  FilmWatched title secret -> (model, request "PUT" secret title FilmWatchedSuccess)
  FilmWatchedSuccess title ->
    ( { model | errorMsg = "", films = model.films |> List.map (\f -> if f.title == title then { f | watched = not f.watched, lastDiff = Nothing } else f) }
    , Cmd.none
    )
  ToggleDeleteMode -> ({ model | deleteMode = not model.deleteMode }, Cmd.none)
  ToggleDateView -> ({ model | dateView = not model.dateView }, Cmd.none)

request : String -> String -> String -> (String -> Msg) -> Cmd Msg
request method secret title successMsg =
  Http.request
    { method = method
    , headers = [ Http.header "Gyursecret" secret ]
    , url = Settings.path ++ Endpoints.filmItemsJson
    , body = Http.stringBody "text" title
    , expect = Http.expectWhatever <| Util.processMessage (always <| successMsg title) Failure
    , timeout = Nothing
    , tracker = Nothing
    }

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
  [ showFilmPanel model
  , showFilms model
  ]

showFilms : Model -> Grid.Column Msg
showFilms model =
  [ div [] (List.indexedMap (showFilm model.dateView model.secret model.deleteMode model.editMode) model.films)
  ] |> Grid.col []

showFilm : Bool -> Password -> Bool -> Bool -> Int -> Film -> Html Msg
showFilm dateView secret deleteMode editMode index film =
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
      addedStr = case film.added of
        Ok date -> Date.toIsoString date
        Err str -> str

      modifiedStr =
        film.lastDiff
        |> Maybe.map Date.toIsoString
        |> Maybe.withDefault "now"

      dateStyle = style "color" "lightgray"
  in
    (  (if editMode && deleteMode then deleteButton else text "")
    :: (if editMode then watchedButton else text "")
    :: text (String.fromInt (index + 1) ++ ". ")
    :: text film.title
    :: (if dateView then span [dateStyle] [text <| " " ++ addedStr] else text "")
    :: (if film.watched then [ text " ✔️ " , span [dateStyle] [text <| if dateView then modifiedStr else ""] ] else [])
    ) |> div []

showFilmPanel : Model -> Grid.Column Msg
showFilmPanel model =
  [ randomFilmSection model.random
  , dateViewPanel
  , editPanel model
  ] |> Grid.col []

dateViewPanel : Html Msg
dateViewPanel =
  Button.button
    [ Button.outlinePrimary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick ToggleDateView
    ] [text "📅"]

randomFilmSection : Maybe Film -> Html Msg
randomFilmSection selected =
  [ Button.button
    [ Button.outlinePrimary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick ChooseRandomFilm
    ] [text "Random Film"]
  , selected |> Maybe.map (text << .title) |> Maybe.withDefault (text "")
  ] |> div []

editPanel  : Model -> Html Msg
editPanel model =
  [ Button.button
    [ if model.editMode then Button.outlineDark else Button.outlineInfo
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick ChangeEditMode
    ] [text <| if model.editMode then "🔙" else "✍️"]
  , div [] <| if model.editMode
    then
      [ newFilmSection model
      , text "Secret: "
      , Input.password [ Input.small, Input.value model.secret, Input.onInput SecretChanged ]
      , br [] []
      , Button.button
        [ Button.outlineDanger
        , Button.attrs [ Spacing.m1 ]
        , Button.onClick ToggleDeleteMode
        ] [text "🗑️"]
      , br [] []
      ]
    else []
  , text model.errorMsg
  ] |> div []

newFilmSection : Model -> Html Msg
newFilmSection model =
  case model.newFilm of
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
        , Button.onClick (AddNewFilm newFilm model.secret)
        ] [text "✔️"]
      , br [] []
      , text "Film: "
      , Input.text [Input.small, Input.value newFilm, Input.onInput NewFilmChanged]
      ] |> div []
