module Films exposing (Model, Msg, init, update, view)

import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.Modal as Modal
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN

import Html exposing (Html, div, text, br, span, h4, hr)
import Json.Decode as Decode exposing (Decoder)
import Html.Attributes exposing (style)
import Browser exposing (Document)
import Time exposing (Month(..))
import List.Extra as List
import Random
import Http

import Types.Language exposing (Language(..))
import Types.Date as Date exposing (Date)
import Endpoints
import Settings
import Util

type alias Password = String

type Mode = Normal | Edit | Delete

type alias Film =
  { title : String
  , watched : Bool
  , added : Result String Date
  , lastDiff : Maybe Date
  }

type alias Model =
  { films : List Film
  , random : Maybe Film
  , newFilm : String
  , secret : String
  , errorMsg : String
  , mode : Mode
  , dateView : Bool
  , suggestionModalVisibility : Modal.Visibility
  , editModalVisibility : Modal.Visibility
  }

initialModel : Model
initialModel =
  { films = []
  , random = Nothing
  , newFilm = ""
  , secret = ""
  , errorMsg = ""
  , mode = Normal
  , dateView = False
  , suggestionModalVisibility = Modal.hidden
  , editModalVisibility = Modal.hidden
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
  | CloseSuggestionModal
  | OpenSuggestionModal
  | EditFailure String
  | CloseEditModal

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
  NewFilmChanged ft -> ({ model | newFilm = ft }, Cmd.none)
  SecretChanged ss -> ({ model | secret = ss }, Cmd.none)
  AddNewFilm title secret -> (model, request "POST" secret title AddNewFilmSuccess Failure)
  AddNewFilmSuccess title ->
    ( { model
      | newFilm = ""
      , errorMsg = ""
      , films = model.films ++ [Film title False (Err "just now") Nothing]
      , suggestionModalVisibility = Modal.hidden
      }
    , Cmd.none
    )
  Failure err -> ({ model | errorMsg = err }, Cmd.none)
  ChangeEditMode ->
    let
      toggleMode : Mode -> Mode
      toggleMode mode = case mode of
        Normal -> Edit
        Edit -> Normal
        Delete -> Normal
    in
      ({ model | mode = toggleMode model.mode, secret = "", errorMsg = "" }, Cmd.none)
  DeleteMovie title secret -> (model, request "DELETE" secret title DeleteMovieSuccess EditFailure)
  DeleteMovieSuccess title -> ({ model | films = model.films |> List.filter (\f -> f.title /= title), errorMsg = "" }, Cmd.none)
  FilmWatched title secret -> (model, request "PUT" secret title FilmWatchedSuccess EditFailure)
  FilmWatchedSuccess title ->
    ( { model
      | errorMsg = ""
      , films = model.films
          |> List.map (\f -> if f.title == title then { f | watched = not f.watched, lastDiff = Nothing } else f)
      }
    , Cmd.none
    )
  ToggleDeleteMode ->
    let
      toggleMode : Mode -> Mode
      toggleMode mode = case mode of
        Normal -> Normal
        Edit -> Delete
        Delete -> Edit
    in
      ({ model | mode = toggleMode model.mode }, Cmd.none)
  ToggleDateView -> ({ model | dateView = not model.dateView }, Cmd.none)
  CloseSuggestionModal -> ({ model | suggestionModalVisibility = Modal.hidden, newFilm = "" }, Cmd.none)
  OpenSuggestionModal -> ({ model | suggestionModalVisibility = Modal.shown, errorMsg = "" }, Cmd.none)
  EditFailure err -> ({ model | editModalVisibility = Modal.shown, errorMsg = err }, Cmd.none)
  CloseEditModal -> ({ model | editModalVisibility = Modal.hidden, errorMsg = "" }, Cmd.none)

request : String -> String -> String -> (String -> Msg) -> (String -> Msg) -> Cmd Msg
request method secret title successMsg failMsg =
  Http.request
    { method = method
    , headers = [ Http.header "Gyursecret" secret ]
    , url = Settings.path ++ Endpoints.filmItemsJson
    , body = Http.stringBody "text" title
    -- TODO implement custom error messages!
    , expect = Http.expectWhatever <| Util.processMessage (always <| successMsg title) failMsg
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
    , [ Modal.config CloseSuggestionModal
        |> Modal.h3 [] [text "Film Javaslat"]
        |> Modal.body [] [suggestionPanel state]
        |> Modal.view state.suggestionModalVisibility
      , Modal.config CloseEditModal
        |> Modal.h3 [] [text "Módosítás"]
        |> Modal.body [] [editModalBody state]
        |> Modal.view state.editModalVisibility
      , showModel state
        |> Grid.row []
      ] |> Grid.container []
    ]
  }

showModel : Model -> List (Grid.Column Msg)
showModel model =
  [ showFilmPanel model
  , showFilms model
  ] |> List.map (\e -> Grid.col [] [e])

showFilms : Model -> Html Msg
showFilms model =
  let
    filmsDivStyle = [style "height" "75vh", style "overflow" "scroll"]
    films = model.films |> List.sortBy (\f -> if f.watched then 1 else 0)
  in

    [ h4 [] [text "Filmek"]
    , div filmsDivStyle (List.indexedMap (showFilm model.dateView model.secret model.mode) films)
    ] |> div []

showFilm : Bool -> Password -> Mode -> Int -> Film -> Html Msg
showFilm dateView secret mode index film =
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
    (  (if mode == Delete then deleteButton else text "")
    :: (if mode == Edit || mode == Delete then watchedButton else text "")
    :: text (String.fromInt (index + 1) ++ ". ")
    :: text film.title
    :: (if dateView then span [dateStyle] [text <| " " ++ addedStr] else text "")
    :: (if film.watched then [ text " ✔️ " , span [dateStyle] [text <| if dateView then modifiedStr else ""] ] else [])
    ) |> div []

showFilmPanel : Model -> Html Msg
showFilmPanel model =
  [ randomFilmSection model.random
  , dateViewPanel
  , filmSuggestionPanel
  , editPanel model
  ] |> div []

filmSuggestionPanel : Html Msg
filmSuggestionPanel =
  [ text "Film Javaslat: "
  , Button.button
    [ Button.outlinePrimary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick OpenSuggestionModal
    ] [text "☝️"]
  ] |> div []

dateViewPanel : Html Msg
dateViewPanel =
  [ text "Dátumos Nézet: "
  , Button.button
    [ Button.outlinePrimary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick ToggleDateView
    ] [text "📅"]
  ] |> div []


randomFilmSection : Maybe Film -> Html Msg
randomFilmSection selected =
  [ h4 [] [text "Film választás"]
  , text "Véletlenszerű (egyenlő esély, nem nézett): "
  , Button.button
    [ Button.outlinePrimary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick ChooseRandomFilm
    ] [text "🎲"]
  , br [] []
  , selected |> Maybe.map (\s -> Alert.simpleSuccess [] [text s.title]) |> Maybe.withDefault (text "")
  , hr [] []
  ] |> div []

editPanel  : Model -> Html Msg
editPanel model =
  let isEditMode = model.mode == Edit || model.mode == Delete
  in
  [ text "Módosítás: "
  , Button.button
      [ if isEditMode then Button.outlineDark else Button.outlineInfo
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick ChangeEditMode
      ] [text <| if isEditMode then "🔙" else "✍️"]
  , if isEditMode
    then
      Button.button
      [ Button.outlineDanger
      , Button.attrs [ Spacing.m1 ]
      , Button.onClick ToggleDeleteMode
      ] [text "🗑️"]
    else text ""
  ] |> div []

suggestionPanel : Model -> Html Msg
suggestionPanel model =
  [ text "Cím"
  , Input.text [Input.small, Input.value model.newFilm, Input.onInput NewFilmChanged]
  , br [] []
  , text "Titok"
  , Input.password [Input.small, Input.value model.secret, Input.onInput SecretChanged]
  , Button.button
    [ Button.outlineSuccess
    , Button.disabled (String.isEmpty model.newFilm || String.isEmpty model.secret)
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick (AddNewFilm model.newFilm model.secret)
    ] [text "Mentés"]
  , br [] []
  , text model.errorMsg
  ] |> div []

editModalBody : Model -> Html Msg
editModalBody model =
  [ text "Titok"
  , Input.password [Input.small, Input.value model.secret, Input.onInput SecretChanged]
  , Button.button
    [ Button.outlineSuccess
    , Button.disabled (String.isEmpty model.secret)
    , Button.attrs [ Spacing.m2 ]
    -- TODO this should repeat the message that failed
    -- if pressed watched, it should send the watched message with the new secret
    -- if pressed delete, same business, just send delete message
    , Button.onClick (AddNewFilm model.newFilm model.secret)
    ] [text "Mentés"]
  , br [] []
  , text model.errorMsg
  ] |> div []
