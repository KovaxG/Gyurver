module BlogList exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Json.Decode as Decode exposing (Decoder)
import Date
import Html exposing (Html, div, br, h3, a, text)
import Html.Attributes exposing (href)
import Time exposing (Month(..))
import Http

import Types.Date as Date
import Types.Language as Language exposing (Language(..))
import Types.Date exposing (Date)
import Endpoints exposing (Endpoint(..))
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
    (Decode.field "identifier" Decode.int)
    (Decode.field "title" Decode.string)
    (Decode.field "date" Date.decode)
    (Decode.field "intro" Decode.string)
    (Decode.field "languages" (Decode.list Language.decoder))
    (Decode.field "topics" (Decode.list Decode.string))

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
update msg _ =
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
  , h3 [] [a [href <| Settings.path ++ Endpoints.show (BlogItemPage blogItem.identity)] [text blogItem.title]]
  , text <| "👅 " ++ showFlags blogItem.languages
  , br [] []
  , text "📅 "
  , text <| Date.toIsoString <| blogItem.date
  , br [] []
  , text "🗜️ "
  , text blogItem.intro
  , br [] []
  , showTag blogItem.topics
  ] |> div []

showTag : List String -> Html Msg
showTag ss =
  if (List.isEmpty ss)
  then text ""
  else div [] (text "🏷️" :: List.map makeBadge ss)

makeBadge : String -> Html Msg
makeBadge str = Badge.badgePrimary [Spacing.ml1] [text str]

showFlags : List Language -> String
showFlags langs = String.concat <| List.map (\l -> Language.flag l ++ " ") langs
