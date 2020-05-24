module VideoAdd exposing (Model, Msg, init, update, view)

import Browser exposing (UrlRequest, Document, application)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Spinner as Spinner
import Html exposing (Html, text, button, h1, p, a, br)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Encode as Json exposing (Value)
import Json.Decode as Decode
import Http as Http exposing (Error(..))
import Date as Date exposing (Date)
import Task
import Time exposing (Month(..))
import Maybe.Extra as Maybe

import Settings

type Status 
  = Editing 
  | Waiting 
  | Received String

type alias Model = 
  { url : String
  , title : String
  , author : String
  , date : String
  , comment : String
  , watchDate : String
  , tags : String
  , status : Status
  }
  
type alias VideoAddRequest =
  { url : String
  , title : String
  , author : String
  , date : Date
  , comment : String
  , watchDate : Maybe Date
  , tags : List String
  }
  
toRequest : Model -> Maybe VideoAddRequest
toRequest model =
  VideoAddRequest
  |> flip Maybe.map (nonEmpty model.url)
  |> Maybe.andMap (nonEmpty model.title)
  |> Maybe.andMap (nonEmpty model.author)
  |> Maybe.andMap (Result.toMaybe <| Date.fromIsoString model.date)
  |> Maybe.andMap (Just model.comment)
  |> Maybe.andMap (Just <| Result.toMaybe <| Date.fromIsoString model.watchDate)
  |> Maybe.andMap (Just <| List.map String.trim <| String.split "," model.tags)
  
nonEmpty : String -> Maybe String
nonEmpty s = if String.isEmpty s then Nothing else Just s
  
isInvalid : Model -> Bool
isInvalid model = case toRequest model of
  Just _ -> model.status /= Editing
  Nothing -> False

type Msg 
  = UrlChanged String
  | TitleChanged String
  | AuthorChanged String
  | DateChanged String
  | CommentChanged String
  | WatchDateChanged String
  | TagsChanged String
  | SaveData
  | Success
  | Response String
  
toMessage : Result Error String -> Msg
toMessage result = 
  Response <| 
    case result of
      Ok msg -> msg
      Err error ->
        case error of
          BadUrl str -> str
          Timeout -> "Request timed out. Check out the server, it might be overloaded."
          NetworkError -> "Network Error. Lol the description says that it means the user turned off their wifi, went in a cave, etc. :))"
          BadStatus code -> "Bad Status with code" ++ String.fromInt code
          BadBody str -> "Bad Body: " ++ str

init : (Model, Cmd Msg)
init = 
  ( { url = ""
    , title = ""
    , author = ""
    , date = ""
    , comment = "No Comment"
    , watchDate = Date.toIsoString <| Date.fromCalendarDate 2020 May 13
    , tags = ""
    , status = Editing
    }
  , Task.perform (WatchDateChanged << Date.toIsoString) Date.today
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  UrlChanged url -> ({ model | url = url }, Cmd.none)
  TitleChanged title -> ({ model | title = title }, Cmd.none)
  AuthorChanged author -> ({ model | author = author }, Cmd.none)
  DateChanged date -> ({ model | date = date }, Cmd.none)
  CommentChanged comment -> ({ model | comment = comment }, Cmd.none)
  WatchDateChanged watchDate -> ({ model | watchDate = watchDate }, Cmd.none)
  TagsChanged tags -> ({ model | tags = tags }, Cmd.none)
  Success -> ({ model | status = Received "OK" }, Cmd.none)
  Response message -> ({ model | status = Received message }, Cmd.none)
  SaveData -> 
    case toRequest model of
      Just request ->
        ( { model | status = Waiting }
        , Http.post
          { url = Settings.path ++ "/api/vids"
          , body = Http.jsonBody (encodeRequest request)
          , expect = Http.expectString toMessage
          }
        )
      Nothing ->
        ({ model | status = Received "Invalid Form. Can not send." }, Cmd.none)
  
view : Model -> Document Msg
view model = 
  { title = "Welcome"
  , body = 
    [ [ CDN.stylesheet
      , h1 [] [text "📼 New Video"]
      , text "URL"
      , urlInput model
      , text "Title"
      , textInput model.title TitleChanged
      , text "Author / Channel"
      , textInput model.author AuthorChanged
      , text "Date of the Video"
      , dateInput model.date DateChanged
      , text "Comment"
      , commentInput model
      , text "Watch Date"
      , dateInput model.watchDate WatchDateChanged
      , text "Tags"
      , textInput model.tags TagsChanged
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
      ] |> Grid.container []
    ] 
  }
  
urlInput : Model -> Html Msg
urlInput model = Input.url 
  [ Input.value model.url 
  , Input.placeholder "http://youtube.com/stuff"
  , Input.onInput UrlChanged
  , if String.isEmpty model.url 
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
  , Input.placeholder "creator or youtube channel goes here"
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
  
commentInput : Model -> Html Msg
commentInput model = Textarea.textarea
  [ Textarea.onInput CommentChanged
  , Textarea.value model.comment
  ]

encodeRequest : VideoAddRequest -> Value
encodeRequest var = 
  Json.object
    [ ("url", Json.string var.url)
    , ("title", Json.string var.title)
    , ("author", Json.string var.author)
    , ("date", encodeDate var.date)
    , ("comment", Json.string var.comment)
    , ("watchDate", Maybe.withDefault Json.null <| Maybe.map encodeDate var.watchDate)
    , ("tags", Json.list Json.string var.tags)
    ]

encodeDate : Date -> Value
encodeDate date =
  Json.object
    [ ("year", Json.int <| Date.year date)
    , ("month", Json.int <| Date.monthNumber date)
    , ("day", Json.int <| Date.day date)
    ]
    
flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b
