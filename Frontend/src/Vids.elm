module Vids exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, text, h1, h3, br, strong, div, iframe, input, span)
import Html.Attributes exposing (src, placeholder, value)
import Html.Events exposing (onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Spinner as Spinner
import Date exposing (Date)
import Time exposing (Month(..))
import Http exposing (Error)
import Settings
import Endpoints
import Json.Decode as Decode exposing (Decoder)
import Set
import Types.Video as Video exposing (Video)
import Types.Result as Result
import Html.Events exposing (onClick)
import Util

type TagType = SelectedTag | NormalTag

type ItemStatus = Viewing | Editing | Waiting

type alias VideoItem =
  { video : Video
  , status : ItemStatus
  , title : String
  , author : String
  , date : String
  , watchDate : String
  , tags : String
  , comment : String
  , error : String
  }

type alias VideoEditRequest = Video

itemToVideoEditRequest : VideoItem -> VideoEditRequest
itemToVideoEditRequest item =
  let video = item.video
  in
    { video
    | title = item.title
    , author = item.author
    , date = Date.fromIsoString item.date |> Result.withDefault video.date
    , watchDate = Date.fromIsoString item.watchDate |> Result.map Just |> Result.withDefault video.watchDate
    , tags = List.map String.trim <| String.split "," item.tags
    , comment = item.comment
    }

newVideoItem : Video -> VideoItem
newVideoItem v =
  { video = v
  , status = Viewing
  , title = v.title
  , author = v.author
  , comment = v.comment
  , date = Date.toIsoString v.date
  , watchDate = Maybe.withDefault "" <| Maybe.map Date.toIsoString v.watchDate
  , tags = String.join ", "  v.tags
  , error = ""
  }

type alias Model =
  { videos : List VideoItem
  , titleFilter : String
  , tagFilter : List String
  }

type VideoEdit
  = TitleChanged String
  | AuthorChanged String
  | CommentChanged String
  | DateChanged String
  | WatchDateChanged String
  | TagsChanged String

type Msg
  = SetVideos (List Video)
  | TitleFilterChanged String
  | AddTag String
  | RemoveTag String
  | Edit Int
  | CancelEdit Int
  | VideoEdit Int VideoEdit
  | SaveChanges Int
  | SaveChangesSuccess Video
  | SaveChangesFailed Int String

init : (Model, Cmd Msg)
init =
  let toMessage : Result Error (List Video) -> Msg
      toMessage result = case result of
        Ok vids -> SetVideos vids
        Err _ -> SetVideos []
  in
    ( { videos = [], titleFilter = "", tagFilter = [] }  -- TODO maybe add a loading state?
    , Http.get
      { url = Settings.path ++ Endpoints.videosJsonEN
      , expect = Http.expectJson toMessage (Decode.list Video.decode)
      }
    )



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  SetVideos vids -> ({ model | videos = List.map newVideoItem vids }, Cmd.none)
  TitleFilterChanged s -> ({ model | titleFilter = s }, Cmd.none)
  AddTag tag -> ({ model | tagFilter = addIfNotContains tag model.tagFilter }, Cmd.none)
  RemoveTag tag -> ({ model | tagFilter = model.tagFilter |> List.filter (\t -> t /= tag) }, Cmd.none)
  Edit nr -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> { item | status = Editing }) model.videos }, Cmd.none)
  CancelEdit nr -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> newVideoItem item.video) model.videos }, Cmd.none)
  SaveChanges nr -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> {item | status = Waiting}) model.videos }, postVideo model nr)
  SaveChangesSuccess video -> ({ model | videos = Util.mapIf (\item -> item.video.nr == video.nr) (\_ -> newVideoItem video) model.videos }, Cmd.none)
  SaveChangesFailed nr str -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> {item | error = str, status = Editing}) model.videos }, Cmd.none)
  VideoEdit nr change ->
    let updateVideo =
          case change of
            TitleChanged str -> (\item -> {item | title = str})
            AuthorChanged str -> (\item -> {item | author = str})
            CommentChanged str -> (\item -> {item | comment = str})
            DateChanged str -> (\item -> {item | date = str})
            WatchDateChanged str -> (\item -> {item | watchDate = str})
            TagsChanged str -> (\item -> {item | tags = str})
    in ({model | videos = Util.mapIf (\item -> item.video.nr == nr) updateVideo model.videos}, Cmd.none)

postVideo : Model -> Int -> Cmd Msg
postVideo model nr =
  let toMessage : Video -> Result Error String -> Msg
      toMessage video result =
        result
        |> Result.map (always <| SaveChangesSuccess video)
        |> Result.mapError (SaveChangesFailed nr << Util.showError)
        |> Result.merge
  in
    model.videos
    |> List.filter (\item -> item.video.nr == nr)
    |> List.head
    |> Maybe.map itemToVideoEditRequest
    |> Maybe.map (\request ->
      Http.post
        { url = Settings.path ++ Endpoints.videoJson nr
        , body = Http.jsonBody (Video.encode request)
        , expect = Http.expectString (toMessage request)
        })
    |> Maybe.withDefault Cmd.none

view : Model -> Document Msg
view model =
  let searchTermMatches vid = String.contains (String.toLower model.titleFilter) (String.toLower vid.title)
      noSelectedTags = List.isEmpty model.tagFilter
      oneTagIsSelectedFromVid vid = List.any (\t -> List.member t model.tagFilter) vid.tags
      filtered = List.filter (\item -> searchTermMatches item.video && (noSelectedTags || oneTagIsSelectedFromVid item.video)) model.videos
  in
    { title = "Videos"
    , body =
      [ CDN.stylesheet
      , Grid.container []
        (h1 [] [text "Videos"] :: searchSection model filtered :: List.map (videoItemToHtml model) filtered)
      ]
    }

searchSection : Model -> List VideoItem -> Html Msg
searchSection model filteredVideos =
  div [] (titleField model :: tagSelection model :: infoField filteredVideos :: [])

infoField : List VideoItem -> Html Msg
infoField items =
  let videos = items |> List.length
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
  let availableTags = model.videos |> List.map .video |> List.concatMap .tags |> Set.fromList |> Set.toList |> List.map (tagToBadge model)
      availableTagSection = text "Tags: " :: availableTags
      takenTagSection = if (List.isEmpty model.tagFilter) then [] else (text "Selected: " :: (List.map (tagToBadge model) model.tagFilter))
  in div []
      [ div [] availableTagSection
      , div [] takenTagSection
      ]

videoItemToHtml : Model -> VideoItem -> Html Msg
videoItemToHtml model item =
  if item.status == Viewing
  then nonEditableVideoToHtml model item.video
  else editableVideoToHtml model item

editableVideoToHtml : Model -> VideoItem -> Html Msg
editableVideoToHtml model item =
  [ Grid.row [] [Grid.col [] [input [ placeholder "title", value item.title, onInput (VideoEdit item.video.nr << TitleChanged)] []]]
  , Grid.row []
    [ Grid.col [] [iframe [src item.video.url] []]
    , Grid.col []
      [ strong [] [text "Channel "]
      , input [ placeholder "author", value item.author, onInput (VideoEdit item.video.nr << AuthorChanged)] []
      , br [] []
      , strong [] [text "Date "]
      , Input.date [ Input.onInput (VideoEdit item.video.nr << DateChanged), Input.value item.date, if String.isEmpty item.date then Input.danger else Input.id ""]
      , br [] []
      , strong [] [text "Watch Date "]
      , Input.date [ Input.onInput (VideoEdit item.video.nr << WatchDateChanged), Input.value item.watchDate]
      , br [] []
      , strong [] [text "Tags "]
      , input [ placeholder "tags", value item.tags, onInput (VideoEdit item.video.nr << TagsChanged)] []
      , br [] []
      , strong [] [text "Nr "]
      , text (String.fromInt item.video.nr)
      , Button.button [Button.onClick (CancelEdit item.video.nr)] [text "🔙"]
      , if item.status == Waiting
        then Spinner.spinner [] []
        else Button.button [Button.onClick (SaveChanges item.video.nr)] [text "💾"]
      , text item.error
      ]
    ]
  , Grid.row []
    [ Grid.col []
      [ strong [] [text "Comment "]
      , Textarea.textarea [Textarea.onInput (VideoEdit item.video.nr << CommentChanged), Textarea.value item.comment]
      ]
    ]
  , br [] []
  ] |> div []


nonEditableVideoToHtml : Model -> Video -> Html Msg
nonEditableVideoToHtml model video =
  [ Grid.row [] [Grid.col [] [h3 [] [text video.title]]]
  , Grid.row []
    [ Grid.col [] [iframe [src video.url] []]
    , Grid.col []
      [ strong [] [text "Channel "]
      , text video.author
      , br [] []
      , strong [] [text "Date "]
      , text (Date.toIsoString video.date)
      , br [] []
      , strong [] [text "Watch Date "]
      , video.watchDate
        |> Maybe.map Date.toIsoString
        |> Maybe.withDefault "¯\\_(ツ)_/¯"
        |> text
      , br [] []
      , div [] (strong [] [text "Tags "] :: List.map (tagToBadge model) video.tags)
      , strong [] [text "Nr "]
      , text (String.fromInt video.nr)
      , Button.button [Button.onClick (Edit video.nr)] [text "✏️"]
      ]
    ]
  , Grid.row []
    [ Grid.col []
      [ strong [] [text "Comment "]
      , text video.comment
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
