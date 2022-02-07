module Blog exposing (Model, Msg, init, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser exposing (Document)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (Html, div, br, h3, a, text, p, hr)
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
  , footnotes : List Footnote
  , references : List Reference
  , metadata : Metadata
  }

type Section = Paragraph String
type Reference = Ref Int String String

type Footnote = Foot String String

type alias Metadata =
  { languages : List Language
  , topics : List String
  }

blogPostDecoder : Decoder BlogPost
blogPostDecoder =
  Decode.map8
    BlogPost
    (Decode.field "identifier" Decode.int)
    (Decode.field "title" Decode.string)
    (Decode.field "date" Date.decode)
    (Decode.field "intro" Decode.string)
    (Decode.field "sections" <| Decode.list sectionDecoder)
    (Decode.field "footnotes" <| Decode.list footnoteDecoder)
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

footnoteDecoder : Decoder Footnote
footnoteDecoder =
  Decode.map2
    Foot
    (Decode.field "symbol" Decode.string)
    (Decode.field "text" Decode.string)

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
displayBlogPost bp =
  h3 [] [text bp.title]
  :: displayInfo bp
  :: (List.map displaySection bp.sections)
  ++ (hr [] [] :: List.map displayFoot bp.footnotes)
  ++ (br [] [] :: List.map displayRef bp.references)
  |> div []

displayInfo : BlogPost -> Html Msg
displayInfo bp =
  ( [ text "👅 "
    , displayFlags bp.metadata.languages
    , br [] []
    , text "📅 "
    , text <| Date.toIsoString bp.date
    , br [] []
    , text "🗜️ "
    , text <| bp.intro
    , br [] []
    ] ++ showTag bp.metadata.topics
  ) |> p []

showTag : List String -> List (Html Msg)
showTag ss = if (List.isEmpty ss) then [] else [text "🏷️ ", displayTopics ss]

displayFlags : List Language -> Html Msg
displayFlags langs = text <| String.concat <| List.map (Language.flag) langs

displayTopics : List String -> Html Msg
displayTopics tags =
  tags
  |> List.intersperse ", "
  |> String.concat
  |> text

displayRef : Reference -> Html Msg
displayRef (Ref i a link) =
  div [] [text <| "[" ++ String.fromInt i ++ "] " ++ a ++ " - " ++ link]

displayFoot : Footnote -> Html Msg
displayFoot (Foot sym txt) =
  div [] [text <| sym ++ " " ++ txt]

displaySection : Section -> Html Msg
displaySection section =
  case section of
    Paragraph s -> p [] [text s]
