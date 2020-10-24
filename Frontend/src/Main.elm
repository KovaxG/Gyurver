module Main exposing (..)

import Browser exposing (UrlRequest(..), Document, application)
import Browser.Navigation exposing (Key)
import Browser.Navigation as Nav
import Html exposing (text)
import Url exposing (Url)
import Debug
import Dict exposing (Dict)

import Landing
import CokkList
import Eredmenyek
import Articles
import VideoAdd
import Vids as VideoList
import Browser.Navigation exposing (pushUrl)

main : Program () Model Msg
main = application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = UrlRequest
  , onUrlChange = UrlChange
  }

type alias Model =
  { content : Content
  , key : Key
  }

type Content
  = Landing Landing.Model
  | CokkList CokkList.Model
  | Eredmenyek Eredmenyek.Model
  | Articles Articles.Model
  | VideoAdd VideoAdd.Model
  | VideoList VideoList.Model
  | Invalid Model Msg
  | Loading
  | Test String

type Msg
  = UrlRequest UrlRequest
  | UrlChange Url
  | LandingMsg Landing.Msg
  | CokkListMsg CokkList.Msg
  | EredmenyekMsg Eredmenyek.Msg
  | ArticlesMsg Articles.Msg
  | VideoAddMsg VideoAdd.Msg
  | VideoListMsg VideoList.Msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : () -> Url -> Key -> (Model, Cmd Msg)
init flags url key = selectPage { content = Loading, key = key } url.path

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.content) of
    (LandingMsg welcomeMsg, Landing welcome) ->
      Landing.update welcomeMsg welcome |> liftModelCmd Landing LandingMsg model

    (CokkListMsg cokkListMsg, CokkList cokkList) ->
      CokkList.update cokkListMsg cokkList |> liftModelCmd CokkList CokkListMsg model

    (EredmenyekMsg eredmenyekMsg, Eredmenyek eredmenyek) ->
      Eredmenyek.update eredmenyekMsg eredmenyek |> liftModelCmd Eredmenyek EredmenyekMsg model

    (ArticlesMsg articlesMsg, Articles articles) ->
      Articles.update articlesMsg articles |> liftModelCmd Articles ArticlesMsg model

    (VideoAddMsg videoAddMsg, VideoAdd videoAdd) ->
      VideoAdd.update videoAddMsg videoAdd |> liftModelCmd VideoAdd VideoAddMsg model

    (VideoListMsg videoListMsg, VideoList videoList) ->
      VideoList.update videoListMsg videoList |> liftModelCmd VideoList VideoListMsg model

    (UrlRequest request, _) ->
      case request of
        External path -> ({ model | content = Loading }, Nav.load path)
        Internal url -> selectPage model url.path

    (UrlChange url, _) ->
      validLinks model
      |> Dict.get url.path
      |> Maybe.withDefault ({ model | content = Loading }, Cmd.none)

    (_, _) -> ({ model | content = Invalid model msg }, Cmd.none)

validLinks : Model -> Dict String (Model, Cmd Msg)
validLinks model = Dict.fromList
  [ ("/", Landing.init |> liftModelCmd Landing LandingMsg model)
  , ("/cokk", CokkList.init |> liftModelCmd CokkList CokkListMsg model)
  , ("/cokk/eredmeny", Eredmenyek.init |> liftModelCmd Eredmenyek EredmenyekMsg  model)
  , ("/articles", Articles.init |> liftModelCmd Articles ArticlesMsg  model)
  , ("/vids", VideoList.init |> liftModelCmd VideoList VideoListMsg model)
  , ("/vids/add", VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model)
  ]

selectPage : Model -> String -> (Model, Cmd Msg)
selectPage model path =
  let loading = { model | content = Loading }
  in
    if Dict.member path (validLinks model)
    then pushNewUrl model.key path (loading, Cmd.none)
    else (loading, Nav.load path)

view : Model -> Document Msg
view model =
  case model.content of
    Landing welcome -> Landing.view welcome |> liftDocument LandingMsg
    CokkList cokkList -> CokkList.view cokkList |> liftDocument CokkListMsg
    Eredmenyek eredmenyek -> Eredmenyek.view eredmenyek |> liftDocument EredmenyekMsg
    Articles articles -> Articles.view articles |> liftDocument ArticlesMsg
    VideoAdd videoAdd -> VideoAdd.view videoAdd |> liftDocument VideoAddMsg
    VideoList videoList -> VideoList.view videoList |> liftDocument VideoListMsg
    Loading -> { title = "Loading", body = [text "Loading..."]}
    Test msg -> { title = "Test", body = [text msg] }
    Invalid md ms ->
      { title = "Error"
      , body = [text <| Debug.toString md ++ " - " ++ Debug.toString ms]
      }

liftDocument : (a -> b) -> Document a -> Document b
liftDocument f da =
  { title = da.title
  , body = List.map (Html.map f) da.body
  }

liftModelCmd : (a -> Content) -> (b -> d) -> Model -> (a, Cmd b) -> (Model, Cmd d)
liftModelCmd fm fc model (m, cmd) =
  ( { model | content = fm m }
  , Cmd.map fc cmd
  )

pushNewUrl : Key -> String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
pushNewUrl key path (model, cmd) =
  (model, Cmd.batch [Nav.pushUrl key path, cmd])
