module BlogList exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Json.Decode as Decode exposing (Decoder)
import Date exposing (fromCalendarDate, toIsoString)
import Html exposing (Html, div, br, h3, a, strong, text)
import Html.Attributes exposing (href)
import Time exposing (Month(..))
import Http

import Types.Date as Date
import Types.Language as Language exposing (Language(..))
import Types.Date exposing (Date)
import Endpoints
import Util
import Settings

type alias BlogItem =
  { identity : Int
  , title : String
  , date : Date
  , intro : String
  , languages : List Language
  , topics : List String
  }

blogItemDecoder : Decoder BlogItem
blogItemDecoder =
  Decode.map6
    BlogItem
    (Decode.field "identity" Decode.int)
    (Decode.field "title" Decode.string)
    (Decode.field "date" Date.decode)
    (Decode.field "intro" Decode.string)
    (Decode.field "languages" (Decode.list Language.decoder))
    (Decode.field "topics" (Decode.list Decode.string))

example =
  [ {identity = 1, title = "test 1", date = fromCalendarDate 2021 Jul 3, intro = "intro 1", languages = [EN], topics = ["test"]}
  , {identity = 2, title = "test 2", date = fromCalendarDate 2021 Jul 10, intro = "intro 1", languages = [EN, HU], topics = ["test", "test1", "omg"]}
  , {identity = 3, title = "test 3", date = fromCalendarDate 2021 Jul 21, intro = "intro 1", languages = [EN], topics = ["test"]}
  , {identity = 4, title = "test 4", date = fromCalendarDate 2021 Aug 5, intro = "intro 1", languages = [RO], topics = ["test"]}
  , {identity = 5, title = "test 5", date = fromCalendarDate 2021 Aug 30, intro = "intro 1", languages = [DE, EN, HU, RO], topics = ["test"]}
  ]

type Model
  = Populated (List BlogItem)
  | ShowMessage String

type Msg
  = Populate (List BlogItem)
  | PopulateError String

init : (Model, Cmd Msg)
init =
  ( ShowMessage "Waiting for server to send me some blogs..."
  , Http.get
    { url = Settings.path ++ Endpoints.blogItemsJson
    , expect =
      Http.expectJson
        (Util.processMessage processIncommingList PopulateError)
        (Decode.list blogItemDecoder)
    }
  )

processIncommingList : List BlogItem -> Msg
processIncommingList bs = if List.isEmpty bs then PopulateError "I got nothing ¯\\_(ツ)_/¯" else Populate bs

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Populate items -> (Populated items, Cmd.none)
    PopulateError error -> (ShowMessage error, Cmd.none)

view : Model -> Document Msg
view state =
  let body =
        case state of
          ShowMessage msg -> [text msg]
          Populated blogItems -> List.map viewBlogItem blogItems
  in
    { title = "Articles"
    , body =
      [ CDN.stylesheet
      , [ [ Grid.col [] body
          ] |> Grid.row []
        ] |> Grid.container []
      ]
    }

viewBlogItem : BlogItem -> Html Msg
viewBlogItem blogItem =
  [ br [] []
  , h3 [] [a [href <| Settings.path ++ Endpoints.blogItemPage ++ String.fromInt blogItem.identity] [text <| showFlags blogItem.languages ++ blogItem.title]]
  , strong [] [text "Date "]
  , text <| toIsoString <| blogItem.date
  , br [] []
  , strong [] [text "Intro "]
  , text blogItem.intro
  , br [] []
  , div [] (strong [] [text "Tags"] :: List.map makeBadge blogItem.topics)
  ] |> div []

makeBadge : String -> Html Msg
makeBadge str = Badge.badgePrimary [Spacing.ml1] [text str]

showFlags : List Language -> String
showFlags langs = String.concat <| List.map (\l -> Language.flag l ++ " ") langs
