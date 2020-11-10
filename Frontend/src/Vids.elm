module Vids exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, text, h1, h3, br, strong, div, iframe, input)
import Html.Attributes exposing (src, placeholder, value)
import Html.Events exposing (onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing as Spacing
import Date exposing (Date)
import Time exposing (Month(..))
import Http exposing (Error)
import Settings
import Endpoints
import Json.Decode as Decode exposing (Decoder)
import Set
import Types.Video as Video exposing (Video)
import Html.Events exposing (onClick)

type TagType = SelectedTag | NormalTag

type alias Model =
  { videos : List Video
  , titleFilter : String
  , tagFilter : List String
  }

type Msg
  = SetVideos (List Video)
  | TitleFilterChanged String
  | AddTag String
  | RemoveTag String

init : (Model, Cmd Msg)
init =
  ( { videos = [], titleFilter = "", tagFilter = [] }  -- TODO maybe add a loading state?
  , Http.get
    { url = Settings.path ++ Endpoints.videosJsonEN
    , expect = Http.expectJson toMessage (Decode.list Video.decode)
    }
  )

toMessage : Result Error (List Video) -> Msg
toMessage result = case result of
  Ok vids -> SetVideos vids
  Err _ -> SetVideos []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  SetVideos vids -> ({ model | videos = vids }, Cmd.none)
  TitleFilterChanged s -> ({ model | titleFilter = s }, Cmd.none)
  AddTag tag -> ({ model | tagFilter = addIfNotContains tag model.tagFilter }, Cmd.none)
  RemoveTag tag -> ({ model | tagFilter = model.tagFilter |> List.filter (\t -> t /= tag) }, Cmd.none)

view : Model -> Document Msg
view model =
  let searchTermMatches vid = String.contains (String.toLower model.titleFilter) (String.toLower vid.title)
      noSelectedTags = List.isEmpty model.tagFilter
      oneTagIsSelectedFromVid vid = List.any (\t -> List.member t model.tagFilter) vid.tags
      filtered = List.filter (\vid -> searchTermMatches vid && (noSelectedTags || oneTagIsSelectedFromVid vid)) model.videos
  in
    { title = "Videos"
    , body =
      [ CDN.stylesheet
      , Grid.container []
        (h1 [] [text "Videos"] :: searchSection model filtered :: List.map (videoToHtml model) filtered)
      ]
    }

searchSection : Model -> List Video -> Html Msg
searchSection model filteredVideos =
  div [] (titleField model :: tagSelection model :: infoField filteredVideos :: [])

infoField : List Video -> Html Msg
infoField vids =
  let videos = vids |> List.length
  in div [] [text <| "Showing " ++ String.fromInt videos ++ " videos"]

titleField : Model -> Html Msg
titleField model =
  div []
    [ text "Search by Title: "
    , input
      [ placeholder "..."
      , value model.titleFilter
      , onInput TitleFilterChanged
      ] []
    ]

tagSelection : Model -> Html Msg
tagSelection model =
  let availableTags = model.videos |> List.concatMap .tags |> Set.fromList |> Set.toList |> List.map (tagToBadge model)
      availableTagSection = text "Tags: " :: availableTags
      takenTagSection = if (List.isEmpty model.tagFilter) then [] else (text "Selected: " :: (List.map (tagToBadge model) model.tagFilter))
  in div []
      [ div [] availableTagSection
      , div [] takenTagSection
      ]

videoToHtml : Model -> Video -> Html Msg
videoToHtml model vid =
  [ Grid.row [] [Grid.col [] [h3 [] [text vid.title]]]
  , Grid.row []
    [ Grid.col [] [iframe [src vid.url] []]
    , Grid.col []
      [ strong [] [text "Channel "]
      , text vid.author
      , br [] []
      , strong [] [text "Date "]
      , text (Date.toIsoString vid.date)
      , br [] []
      , strong [] [text "Watch Date "]
      , vid.watchDate
        |> Maybe.map Date.toIsoString
        |> Maybe.withDefault "¯\\_(ツ)_/¯"
        |> text
      , br [] []
      , div [] (strong [] [text "Tags "] :: List.map (tagToBadge model) vid.tags)
      ]
    ]
  , Grid.row []
    [ Grid.col []
      [ strong [] [text "Comment "]
      , text vid.comment
      ]
    ]
  , br [] []
  ] |> div []

tagToBadge : Model -> String -> Html Msg
tagToBadge model tag =
  if List.member tag model.tagFilter
  then Badge.badgeWarning [Spacing.ml1, onClick (RemoveTag tag)] [text tag]
  else Badge.badgePrimary [Spacing.ml1, onClick (AddTag tag)] [text tag]

addIfNotContains : a -> List a -> List a
addIfNotContains x xs = if List.member x xs then xs else xs ++ [x]
