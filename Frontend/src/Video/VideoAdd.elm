module Video.VideoAdd exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Spinner as Spinner
import Bootstrap.Form.Checkbox as Checkbox
import Browser.Navigation as Nav
import Html exposing (Html, text, h1, a, br, iframe, div)
import Html.Attributes exposing (src)
import Http as Http exposing (Error(..))
import Date
import Task
import Time exposing (Month(..))
import Maybe.Extra as Maybe
import List.Extra as List

import Settings
import Endpoints exposing (Endpoint(..))
import Video.NewVideoRequest as NewVideoRequest exposing (NewVideoRequest)
import Types.Result as Result
import Util

type Status
  = Editing
  | Waiting
  | Received String

type alias Model =
  { urlRaw : String
  , url : String
  , title : String
  , author : String
  , date : String
  , comment : String
  , watchDate : Maybe String
  , tags : String
  , status : Status
  , secret : String
  , currentDate : String
  }

toRequest : Model -> Maybe NewVideoRequest
toRequest model =
  NewVideoRequest
  |> flip Maybe.map (nonEmpty model.url)
  |> Maybe.andMap (nonEmpty model.title)
  |> Maybe.andMap (nonEmpty model.author)
  |> Maybe.andMap (Result.toMaybe <| Date.fromIsoString model.date)
  |> Maybe.andMap (Just model.comment)
  |> Maybe.andMap (Just <| Maybe.andThen (Result.toMaybe << Date.fromIsoString) model.watchDate)
  |> Maybe.andMap (Just <| List.map String.trim <| String.split "," model.tags)
  |> Maybe.andMap (Just model.secret)

nonEmpty : String -> Maybe String
nonEmpty s = if String.isEmpty s then Nothing else Just s

isInvalid : Model -> Bool
isInvalid model = case toRequest model of
  Just _ -> model.status == Waiting
  Nothing -> False

type Msg
  = UrlChanged String
  | TitleChanged String
  | AuthorChanged String
  | DateChanged String
  | CommentChanged String
  | WatchDateShow Bool
  | WatchDateChanged String
  | TagsChanged String
  | SecretChanged String
  | SaveData
  | Success
  | Response String
  | GotCurrentDate String

init : (Model, Cmd Msg)
init =
  ( { urlRaw = ""
    , url = ""
    , title = ""
    , author = ""
    , date = ""
    , comment = "No Comment"
    , watchDate = Just <| Date.toIsoString <| Date.fromCalendarDate 2020 May 13
    , tags = ""
    , status = Editing
    , secret = ""
    , currentDate = Date.toIsoString <| Date.fromCalendarDate 2021 Aug 24
    }
  , Task.perform (GotCurrentDate << Date.toIsoString) Date.today
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  UrlChanged url -> ({ model | urlRaw = url, url = Maybe.withDefault url <| parseEmbeddedURL url }, Cmd.none)
  TitleChanged title -> ({ model | title = title }, Cmd.none)
  AuthorChanged author -> ({ model | author = author }, Cmd.none)
  DateChanged date -> ({ model | date = date }, Cmd.none)
  CommentChanged comment -> ({ model | comment = comment }, Cmd.none)
  GotCurrentDate date -> ({ model | watchDate = Just date, currentDate = date }, Cmd.none)
  WatchDateShow b -> ({ model | watchDate = if b then Just model.currentDate else Nothing }, Cmd.none)
  WatchDateChanged watchDate -> ({ model | watchDate = Just watchDate }, Cmd.none)
  TagsChanged tags -> ({ model | tags = tags }, Cmd.none)
  SecretChanged pass -> ({ model | secret = pass }, Cmd.none)
  Success -> ({ model | status = Received "OK" }, Nav.load <| Endpoints.show VideosPage)
  Response message -> ({ model | status = Received message }, Cmd.none)
  SaveData ->
    case toRequest model of
      Just request ->
        ( { model | status = Waiting }
        , Http.post
          { url = Settings.path ++ Endpoints.videosJsonEN
          , body = Http.jsonBody (NewVideoRequest.encode request)
          , expect = Http.expectString <| Util.processMessage (always Success) Response
          }
        )
      Nothing ->
      -- TODO this message should appear before pressing the button!
        ({ model | status = Received "Invalid Form. Can not send." }, Cmd.none)

view : Model -> Document Msg
view model =
  { title = "Welcome"
  , body =
    [ [ CDN.stylesheet
      , [ [ h1 [] [text "📼 New Video"]
          , text "URL"
          , urlInput model
          , text "Title"
          , textInput model.title TitleChanged
          , text "Author / Channel"
          , authorInput model
          , text "Date of the Video"
          , dateInput model.date DateChanged
          , text "Comment"
          , commentInput model
          , text "Watch Date"
          , maybeDateInput model.watchDate WatchDateChanged WatchDateShow
          , text "Tags"
          , textInput model.tags TagsChanged
          , text "Password"
          , passwordInput model
          , Button.button
            [ Button.primary
            , Button.attrs [ Spacing.m2 ]
            , Button.disabled (isInvalid model)
            , Button.onClick SaveData
            ]
            [ if model.status == Waiting
              then Spinner.spinner
                    [ Spinner.small, Spinner.attrs [ Spacing.mr1 ]  ]
                    []
              else text ""
            , text "Save"
            ]
          , br [] []
          , case model.status of
              Received msg -> text msg
              _ -> text ""
          ] |> Grid.col [],
          [ [ text "Preview"
            , br [] []
            , text <| "🔗 " ++ model.url
            , br [] []
            , iframe [src model.url] []
            ] |> div []
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

urlInput : Model -> Html Msg
urlInput model = Input.url
  [ Input.value model.urlRaw
  , Input.placeholder "http://youtube.com/stuff"
  , Input.onInput UrlChanged
  , if String.isEmpty model.urlRaw
    then Input.danger
    else Input.id ""
  ]

textInput : String -> (String -> Msg) -> Html Msg
textInput value msg = Input.text
  [ Input.value value
  , Input.onInput msg
  , if String.isEmpty value
    then Input.danger
    else Input.id ""
  ]

authorInput : Model -> Html Msg
authorInput model = Input.text
  [ Input.value model.author
  , Input.placeholder "Le Artist"
  , Input.onInput AuthorChanged
  , if String.isEmpty model.author
    then Input.danger
    else Input.id ""
  ]

dateInput : String -> (String -> Msg) -> Html Msg
dateInput value msg = Input.date
  [ Input.onInput msg
  , Input.value value
  , if String.isEmpty value
    then Input.danger
    else Input.id ""
  ]

maybeDateInput : Maybe String -> (String -> Msg) -> (Bool -> Msg) -> Html Msg
maybeDateInput valueMaybe dateChange checkMark =
  [ Checkbox.checkbox [ Checkbox.onCheck checkMark, Checkbox.checked (Maybe.isJust valueMaybe)] "I remember when I watched it"
  , case valueMaybe of
      Nothing -> text ""
      Just value ->
        Input.date
          [ Input.onInput dateChange
          , Input.value value
          , if String.isEmpty value
            then Input.danger
            else Input.id ""
          ]
    ] |> div []

commentInput : Model -> Html Msg
commentInput model = Textarea.textarea
  [ Textarea.onInput CommentChanged
  , Textarea.value model.comment
  ]

passwordInput : Model -> Html Msg
passwordInput model = Input.password
  [ Input.onInput SecretChanged
  , Input.value model.secret
  ]

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

parseEmbeddedURL : String -> Maybe String
parseEmbeddedURL =
  String.words
  >> List.find (String.startsWith "src")
  >> Maybe.map (String.dropRight 1 << String.dropLeft 5)
