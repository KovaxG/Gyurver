module Video.Vids exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, text, h1, h3, br, strong, div, iframe, input)
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
import Time exposing (Month(..))
import Http exposing (Error)
import Json.Encode as Encode exposing (Value)
import Date as ElmDate
import Types.Date as Date exposing (Date)
import Settings
import Endpoints
import Json.Decode as Decode
import Set
import Video.Video as Video exposing (Video)
import Types.Result as Result
import Html.Events exposing (onClick)
import Util

type ItemStatus = Viewing | Editing | EditWaiting | Deleting | DeleteWaiting

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
  , password : String
  }

type alias VideoEditRequest =
  { link : String
  , title : String
  , author : String
  , date : Date
  , comment : String
  , watchDate : Maybe Date
  , tags : List String
  , password : String
  }

type alias VideoDeleteRequest = { password : String }

itemToVideoDeleteRequest : VideoItem -> VideoDeleteRequest
itemToVideoDeleteRequest item = { password = item.password }

encodeDeleteRequest : VideoDeleteRequest -> Value
encodeDeleteRequest req = Encode.object [("password", Encode.string req.password)]

encode : VideoEditRequest -> Value
encode req =
  Encode.object
    [ ("url", Encode.string req.link)
    , ("title", Encode.string req.title)
    , ("author", Encode.string req.author)
    , ("date", Date.encode req.date)
    , ("comment", Encode.string req.comment)
    , ("watchDate", Maybe.withDefault Encode.null <| Maybe.map Date.encode req.watchDate)
    , ("tags", Encode.list Encode.string req.tags)
    , ("password", Encode.string req.password)
    ]

itemToVideoEditRequest : VideoItem -> VideoEditRequest
itemToVideoEditRequest item =
  { link = item.video.url
  , title = item.title
  , author = item.author
  , date = ElmDate.fromIsoString item.date |> Result.withDefault item.video.date
  , watchDate = ElmDate.fromIsoString item.watchDate |> Result.map Just |> Result.withDefault item.video.watchDate
  , tags = List.map String.trim <| String.split "," item.tags
  , comment = item.comment
  , password = item.password
  }

videoEditRequestToVideo : Int -> VideoEditRequest -> Video
videoEditRequestToVideo nr ver =
  { nr = nr
  , url = ver.link
  , title = ver.title
  , author = ver.author
  , date = ver.date
  , comment = ver.comment
  , watchDate = ver.watchDate
  , tags = ver.tags
  }

newVideoItem : Video -> VideoItem
newVideoItem v =
  { video = v
  , status = Viewing
  , title = v.title
  , author = v.author
  , comment = v.comment
  , date = ElmDate.toIsoString v.date
  , watchDate = Maybe.withDefault "" <| Maybe.map ElmDate.toIsoString v.watchDate
  , tags = String.join ", "  v.tags
  , error = ""
  , password = ""
  }

type alias Model =
  { videos : List VideoItem
  , titleFilter : String
  , tagFilter : List String
  }

type VideoDelete
 = PasswordChanged String

type VideoEdit
  = TitleChanged String
  | AuthorChanged String
  | CommentChanged String
  | DateChanged String
  | WatchDateChanged String
  | TagsChanged String
  | PasswordCanged String

type Msg
  = SetVideos (List Video)
  | TitleFilterChanged String
  | AddTag String
  | RemoveTag String
  | Edit Int
  | CancelEdit Int
  | VideoEdit Int VideoEdit
  | Delete Int
  | CancelDelete Int
  | VideoDelete Int VideoDelete
  | PerformDeletion Int
  | SaveChanges Int
  | SaveChangesSuccess Video
  | SaveChangesFailed Int String
  | DeleteSuccess Int
  | DeleteFailed Int String

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

  Delete nr -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> { item | status = Deleting }) model.videos }, Cmd.none)
  CancelDelete nr -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> newVideoItem item.video) model.videos }, Cmd.none)

  SaveChanges nr -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> {item | status = EditWaiting}) model.videos }, postVideo model nr)
  SaveChangesSuccess video -> ({ model | videos = Util.mapIf (\item -> item.video.nr == video.nr) (\_ -> newVideoItem video) model.videos }, Cmd.none)
  SaveChangesFailed nr str -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> {item | error = str, status = Editing}) model.videos }, Cmd.none)

  PerformDeletion nr -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> {item | status = DeleteWaiting}) model.videos }, deleteVideo model nr)
  DeleteSuccess nr -> ({ model | videos = List.filter (\item -> item.video.nr /= nr) model.videos }, Cmd.none)
  DeleteFailed nr str -> ({ model | videos = Util.mapIf (\item -> item.video.nr == nr) (\item -> {item | error = str, status = Deleting}) model.videos }, Cmd.none)

  VideoDelete nr change ->
    let updateVideo =
          case change of
            PasswordChanged str -> (\item -> {item | password = str})
    in ({model | videos = Util.mapIf (\item -> item.video.nr == nr) updateVideo model.videos}, Cmd.none)

  VideoEdit nr change ->
    let updateVideo =
          case change of
            TitleChanged str -> (\item -> {item | title = str})
            AuthorChanged str -> (\item -> {item | author = str})
            CommentChanged str -> (\item -> {item | comment = str})
            DateChanged str -> (\item -> {item | date = str})
            WatchDateChanged str -> (\item -> {item | watchDate = str})
            TagsChanged str -> (\item -> {item | tags = str})
            PasswordCanged str -> (\item -> {item | password = str})
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
        , body = Http.jsonBody (encode request)
        , expect = Http.expectString (toMessage <| videoEditRequestToVideo nr request)
        })
    |> Maybe.withDefault Cmd.none

deleteVideo : Model -> Int -> Cmd Msg
deleteVideo model nr =
  let toMessage : Result Error String -> Msg
      toMessage result =
        result
        |> Result.map (always <| DeleteSuccess nr)
        |> Result.mapError (DeleteFailed nr << Util.showError)
        |> Result.merge
  in
    model.videos
    |> List.filter (\item -> item.video.nr == nr)
    |> List.head
    |> Maybe.map itemToVideoDeleteRequest
    |> Maybe.map (\request ->
      Http.request
        { method = "DELETE"
        , headers = []
        , url = Settings.path ++ Endpoints.videoJson nr
        , body = Http.jsonBody (encodeDeleteRequest request)
        , expect = Http.expectString toMessage
        , timeout = Nothing
        , tracker = Nothing
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
  div [] [titleField model, tagSelection model, infoField filteredVideos]

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
  case item.status of
    Viewing -> nonEditableVideoToHtml model item.video
    Editing -> editableVideoToHtml model item
    EditWaiting -> editableVideoToHtml model item
    Deleting -> deleteVideoToHtml model item
    DeleteWaiting -> editableVideoToHtml model item

deleteVideoToHtml : Model -> VideoItem -> Html Msg
deleteVideoToHtml model item =
  [ Grid.row [] [Grid.col [] [h3 [] [text item.video.title]]]
  , Grid.row []
    [ Grid.col [] [iframe [src item.video.url] []]
    , Grid.col []
      [ strong [] [text "URL "]
      , text item.video.url
      , br [] []
      , strong [] [text "Channel "]
      , text item.video.author
      , br [] []
      , strong [] [text "Date "]
      , text (ElmDate.toIsoString item.video.date)
      , br [] []
      , strong [] [text "Watch Date "]
      , item.video.watchDate
        |> Maybe.map ElmDate.toIsoString
        |> Maybe.withDefault "¯\\_(ツ)_/¯"
        |> text
      , br [] []
      , div [] (strong [] [text "Tags "] :: List.map (tagToBadge model) item.video.tags)
      , strong [] [text "Nr "]
      , text (String.fromInt item.video.nr)
      ]
    ]
  , Grid.row []
    [ Grid.col []
      [ strong [] [text "Comment "]
      , text item.video.comment
      , br [] []
      , strong [] [text "Password "]
      , Input.password [Input.onInput (VideoDelete item.video.nr << PasswordChanged), Input.value item.password]
      , div [] [text item.error]
      , Button.button [Button.onClick (CancelDelete item.video.nr)] [text "🔙"]
      , Button.button [Button.onClick (PerformDeletion item.video.nr)] [text "❌"]
      ]
    ]
  , br [] []
  ] |> div []

editableVideoToHtml : Model -> VideoItem -> Html Msg
editableVideoToHtml _ item =
  [ Grid.row [] [Grid.col [] [input [ placeholder "title", value item.title, onInput (VideoEdit item.video.nr << TitleChanged)] []]]
  , Grid.row []
    [ Grid.col [] [iframe [src item.video.url] []]
    , Grid.col []
      [ strong [] [text "URL "]
      , text item.video.url
      , br [] []
      , strong [] [text "Channel "]
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
      ]
    ]
  , Grid.row []
    [ Grid.col []
      [ strong [] [text "Comment "]
      , Textarea.textarea [Textarea.onInput (VideoEdit item.video.nr << CommentChanged), Textarea.value item.comment]
      , strong [] [text "Password "]
      , Input.password [Input.onInput (VideoEdit item.video.nr << PasswordCanged), Input.value item.password]
      , div [] [text item.error]
      , Button.button [Button.onClick (CancelEdit item.video.nr)] [text "🔙"]
      , if item.status == EditWaiting
        then Spinner.spinner [] []
        else Button.button [Button.onClick (SaveChanges item.video.nr)] [text "💾"]
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
      [ strong [] [text "URL "]
      , text video.url
      , br [] []
      , strong [] [text "Channel "]
      , text video.author
      , br [] []
      , strong [] [text "Date "]
      , text (ElmDate.toIsoString video.date)
      , br [] []
      , strong [] [text "Watch Date "]
      , video.watchDate
        |> Maybe.map ElmDate.toIsoString
        |> Maybe.withDefault "¯\\_(ツ)_/¯"
        |> text
      , br [] []
      , div [] (strong [] [text "Tags "] :: List.map (tagToBadge model) video.tags)
      , strong [] [text "Nr "]
      , text (String.fromInt video.nr)
      , Button.button [Button.onClick (Edit video.nr)] [text "✏️"]
      , Button.button [Button.onClick (Delete video.nr)] [text "🗑️"]
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
