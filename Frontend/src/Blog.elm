module Blog exposing (Model, Msg, init, update, view)

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

type alias BlogPost =
  { identifier : Int
  , title : String
  , date : Date
  , intro : String
  , sections : List Section
  , references : List Reference
  , metadata : Metadata
  }

type Section = Paragraph String
type Reference = Ref Int String String

type alias Metadata =
  { languages : List Language
  , topics : List String
  }

blogPostDecoder : Decoder BlogPost
blogPostDecoder =
  Decode.map7
    BlogPost
    (Decode.field "identifier" Decode.int)
    (Decode.field "title" Decode.string)
    (Decode.field "date" Date.decode)
    (Decode.field "intro" Decode.string)
    (Decode.field "sections" <| Decode.list sectionDecoder)
    (Decode.field "references" <| Decode.list referenceDecoder)
    (Decode.field "metadata" metadataDecoder)

sectionDecoder : Decoder Section
sectionDecoder =
  Decode.map2
    (always Paragraph)
    (Decode.field "section" Decode.string)
    (Decode.field "content" Decode.string)

referenceDecoder : Decoder Reference
referenceDecoder =
  Decode.map3
    Ref
    (Decode.field "index" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "url" Decode.string)

metadataDecoder : Decoder Metadata
metadataDecoder =
  Decode.map2
    Metadata
    (Decode.field "languages" <| Decode.list Language.decoder)
    (Decode.field "topics" <| Decode.list Decode.string)

type Model
  = ShowMessage String
  | Ok BlogPost

type Msg
  = PopulateBlog BlogPost
  | PopulateFailed String

init : Int -> (Model, Cmd Msg)
init index =
  ( ShowMessage "Waiting for server to send me some blogs..."
  , Http.get
    { url = Settings.path ++ Endpoints.blogItemJson ++ String.fromInt index
    , expect =
      Http.expectJson
        (Util.processMessage PopulateBlog PopulateFailed)
        blogPostDecoder
    }
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    PopulateBlog b -> (Ok b, Cmd.none)
    PopulateFailed s -> (ShowMessage s, Cmd.none)

view : Model -> Document Msg
view state =
  { title = "Articles"
  , body =
    [ CDN.stylesheet
    , [ [ [ case state of
              ShowMessage m -> displayMessage m
              Ok bp -> displayBlogPost bp
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

displayMessage : String -> Html Msg
displayMessage s = text <| "Error: " ++ s

displayBlogPost : BlogPost -> Html Msg
displayBlogPost bp = text <| Debug.toString bp
