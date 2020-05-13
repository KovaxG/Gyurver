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
  
isInvalid : Model -> Bool
isInvalid model =
  String.isEmpty model.url
  || String.isEmpty model.title
  || String.isEmpty model.author
  || String.isEmpty model.watchDate
  || String.isEmpty model.tags
  || model.status /= Editing

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
    ( { model | status = Waiting }
    , Http.post
      { url = "http://totallysafelink.xyz/vids"
      , body = Http.jsonBody (encodeModel model)
      , expect = Http.expectJson toMessage Decode.string
      }
    )
  
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

encodeModel : Model -> Value
encodeModel model = 
  Json.object
    [ ("url", Json.string model.url)
    , ("title", Json.string model.title)
    , ("author", Json.string model.author)
    , ("date", Json.string model.date)
    , ("comment", Json.string model.comment)
    , ("watchDate", Json.string model.watchDate)
    , ("tags", Json.string model.tags)
    ]
