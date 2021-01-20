module Main exposing (..)

import Browser exposing (UrlRequest(..), Document, application)
import Browser.Navigation exposing (Key)
import Browser.Navigation as Nav
import Html exposing (text)
import Url exposing (Url)
import Debug
import Dict exposing (Dict)

import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Html exposing (Html)
import Html.Attributes exposing (href)
import Bootstrap.Utilities.Spacing as Spacing

import Landing
import CokkList
import Eredmenyek
import Articles
import VideoAdd
import Vids as VideoList
import Browser.Navigation exposing (pushUrl)
import Settings
import Endpoints

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
  , navbar : Navbar.State
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
  = NavbarMsg Navbar.State
  | UrlRequest UrlRequest
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
init flags url key =
  let (navbarInitialState, navbarCmd) = Navbar.initialState NavbarMsg
      (initialModel, initialCmd) = selectPage { content = Loading, key = key, navbar = navbarInitialState } url.path
  in (initialModel, Cmd.batch [initialCmd, navbarCmd])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.content) of
    (LandingMsg wmsg, Landing w) -> Landing.update wmsg w |> liftModelCmd Landing LandingMsg model
    (CokkListMsg clmsg, CokkList cl) -> CokkList.update clmsg cl |> liftModelCmd CokkList CokkListMsg model
    (EredmenyekMsg emsg, Eredmenyek e) -> Eredmenyek.update emsg e |> liftModelCmd Eredmenyek EredmenyekMsg model
    (ArticlesMsg amsg, Articles a) -> Articles.update amsg a |> liftModelCmd Articles ArticlesMsg model
    (VideoAddMsg vamsg, VideoAdd va) -> VideoAdd.update vamsg va |> liftModelCmd VideoAdd VideoAddMsg model
    (VideoListMsg vlmsg, VideoList vl) -> VideoList.update vlmsg vl |> liftModelCmd VideoList VideoListMsg model

    (UrlRequest request, _) ->
      case request of
        External path -> ({ model | content = Loading }, Nav.load path)
        Internal url -> selectPage model url.path

    (UrlChange url, _) ->
      validLinks model
      |> Dict.get url.path
      |> Maybe.withDefault ({ model | content = Loading }, Cmd.none)

    (NavbarMsg newState, _) -> ({ model | navbar = newState }, Cmd.none)

    (_, _) -> ({ model | content = Invalid model msg }, Cmd.none)

validLinks : Model -> Dict String (Model, Cmd Msg)
validLinks model = Dict.fromList
  [ (Endpoints.landingPage, Landing.init |> liftModelCmd Landing LandingMsg model)
  , (Endpoints.cokk2020Page, CokkList.init |> liftModelCmd CokkList CokkListMsg model)
  , (Endpoints.cokk2020ResultsPageEN, Eredmenyek.init |> liftModelCmd Eredmenyek EredmenyekMsg  model)
  , (Endpoints.cokk2020ResultsPageHU, Eredmenyek.init |> liftModelCmd Eredmenyek EredmenyekMsg  model)
  , (Endpoints.cokk2020ResultsPageRO, Eredmenyek.init |> liftModelCmd Eredmenyek EredmenyekMsg  model)
  , (Endpoints.articlesPageEN, Articles.init |> liftModelCmd Articles ArticlesMsg  model)
  , (Endpoints.articlesPageHU, Articles.init |> liftModelCmd Articles ArticlesMsg  model)
  , (Endpoints.articlesPageRO, Articles.init |> liftModelCmd Articles ArticlesMsg  model)
  , (Endpoints.videosPageEN, VideoList.init |> liftModelCmd VideoList VideoListMsg model)
  , (Endpoints.videosPageHU, VideoList.init |> liftModelCmd VideoList VideoListMsg model)
  , (Endpoints.videosPageRO, VideoList.init |> liftModelCmd VideoList VideoListMsg model)
  , (Endpoints.videoAddPageEN, VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model)
  , (Endpoints.videoAddPageHU, VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model)
  , (Endpoints.videoAddPageRO, VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model)
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
    Landing welcome -> Landing.view welcome |> liftDocument model LandingMsg
    CokkList cokkList -> CokkList.view cokkList |> liftDocument model CokkListMsg
    Eredmenyek eredmenyek -> Eredmenyek.view eredmenyek |> liftDocument model EredmenyekMsg
    Articles articles -> Articles.view articles |> liftDocument model ArticlesMsg
    VideoAdd videoAdd -> VideoAdd.view videoAdd |> liftDocument model VideoAddMsg
    VideoList videoList -> VideoList.view videoList |> liftDocument model VideoListMsg
    Loading -> { title = "Loading", body = [text "Loading..."]}
    Test msg -> { title = "Test", body = [text msg] }
    Invalid md ms ->
      { title = "Error"
      , body = [text <| Debug.toString md ++ " - " ++ Debug.toString ms]
      }

navbar : Model -> Html Msg
navbar model =
  Navbar.config NavbarMsg
  |> Navbar.withAnimation
  |> Navbar.brand [ href Endpoints.landingPage ] [ text "Gyurver"]
  |> Navbar.items
    [ Navbar.itemLink [ href Endpoints.articlesPageEN ] [ text "📑 Articles"]
    , Navbar.itemLink [ href Endpoints.videosPageEN ] [ text "📼 Videos"]
    , Navbar.dropdown
      { id = "Cokkolo_Dropdown"
      , toggle = Navbar.dropdownToggle [] [text "🥚 Cökkölő"]
      , items =
        [ Navbar.dropdownItem [href Endpoints.cokk2020Page] [text "2020"]
        , Navbar.dropdownItem [href Endpoints.cokk2021Page] [text "2021"]
        ]
      }
    ]
  |> Navbar.customItems [Navbar.textItem [ ] [ text <| "v" ++ Settings.version]]
  |> Navbar.view model.navbar


liftDocument : Model -> (a -> Msg) -> Document a -> Document Msg
liftDocument model f da =
  { title = da.title
  , body = Grid.container [] [navbar model] :: List.map (Html.map f) da.body
  }

liftModelCmd : (a -> Content) -> (b -> d) -> Model -> (a, Cmd b) -> (Model, Cmd d)
liftModelCmd fm fc model (m, cmd) =
  ( { model | content = fm m }
  , Cmd.map fc cmd
  )

pushNewUrl : Key -> String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
pushNewUrl key path (model, cmd) =
  (model, Cmd.batch [Nav.pushUrl key path, cmd])
