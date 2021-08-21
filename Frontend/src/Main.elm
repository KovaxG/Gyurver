module Main exposing (..)

import Browser exposing (UrlRequest(..), Document, application)
import Browser.Navigation exposing (Key)
import Browser.Navigation as Nav
import Html exposing (text)
import Url exposing (Url)
import Maybe.Extra as Maybe
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Html exposing (Html)
import Html.Attributes exposing (href)

import Landing
import Cokkolo2020.Landing
import Cokkolo2020.Results
import Cokkolo2021.Landing
import Cokkolo2021.Results
import Articles
import BlogList
import Blog
import Films
import Video.VideoAdd as VideoAdd
import Video.Vids as VideoList
import Settings
import Endpoints exposing (Endpoint(..))

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
  | CokkoloLanding2020 Cokkolo2020.Landing.Model
  | CokkoloResults2020 Cokkolo2020.Results.Model
  | CokkoloLanding2021 Cokkolo2021.Landing.Model
  | CokkoloResults2021 Cokkolo2021.Results.Model
  | Articles Articles.Model
  | VideoAdd VideoAdd.Model
  | VideoList VideoList.Model
  | BlogList BlogList.Model
  | Blog Blog.Model
  | Films Films.Model
  | Invalid Model Msg
  | Loading
  | Test String

type Msg
  = NavbarMsg Navbar.State
  | UrlRequest UrlRequest
  | UrlChange Url
  | LandingMsg Landing.Msg
  | CokkoloLanding2020Msg Cokkolo2020.Landing.Msg
  | CokkoloResults2020Msg Cokkolo2020.Results.Msg
  | CokkoloLanding2021Msg Cokkolo2021.Landing.Msg
  | CokkoloResults2021Msg Cokkolo2021.Results.Msg
  | ArticlesMsg Articles.Msg
  | VideoAddMsg VideoAdd.Msg
  | VideoListMsg VideoList.Msg
  | BlogListMsg BlogList.Msg
  | FilmsMsg Films.Msg
  | BlogMsg Blog.Msg

subscriptions : Model -> Sub Msg
subscriptions model = Navbar.subscriptions model.navbar NavbarMsg

init : () -> Url -> Key -> (Model, Cmd Msg)
init _ url key =
  let (navbarInitialState, navbarCmd) = Navbar.initialState NavbarMsg
      (initialModel, initialCmd) = selectPage { content = Loading, key = key, navbar = navbarInitialState } url.path
  in (initialModel, Cmd.batch [initialCmd, navbarCmd])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model.content) of
    (LandingMsg wmsg, Landing w) -> Landing.update wmsg w |> liftModelCmd Landing LandingMsg model
    (CokkoloLanding2020Msg clm, CokkoloLanding2020 cl) -> Cokkolo2020.Landing.update clm cl |> liftModelCmd CokkoloLanding2020 CokkoloLanding2020Msg model
    (CokkoloResults2020Msg crm, CokkoloResults2020 cr) -> Cokkolo2020.Results.update crm cr |> liftModelCmd CokkoloResults2020 CokkoloResults2020Msg model
    (CokkoloLanding2021Msg clm, CokkoloLanding2021 cl) -> Cokkolo2021.Landing.update clm cl |> liftModelCmd CokkoloLanding2021 CokkoloLanding2021Msg model
    (CokkoloResults2021Msg crm, CokkoloResults2021 cr) -> Cokkolo2021.Results.update crm cr |> liftModelCmd CokkoloResults2021 CokkoloResults2021Msg model
    (ArticlesMsg amsg, Articles a) -> Articles.update amsg a |> liftModelCmd Articles ArticlesMsg model
    (VideoAddMsg vamsg, VideoAdd va) -> VideoAdd.update vamsg va |> liftModelCmd VideoAdd VideoAddMsg model
    (VideoListMsg vlmsg, VideoList vl) -> VideoList.update vlmsg vl |> liftModelCmd VideoList VideoListMsg model
    (BlogListMsg bmsg, BlogList b) -> BlogList.update bmsg b |> liftModelCmd BlogList BlogListMsg model
    (BlogMsg bmsg, Blog b) -> Blog.update bmsg b |> liftModelCmd Blog BlogMsg model
    (FilmsMsg fmsg, Films f) -> Films.update fmsg f |> liftModelCmd Films FilmsMsg model

    (UrlRequest request, _) ->
      case request of
        External path -> ({ model | content = Loading }, Nav.load path)
        Internal url -> selectPage model url.path

    (UrlChange url, _) -> validLinks model url.path |> Maybe.withDefault ({ model | content = Loading }, Cmd.none)

    (NavbarMsg newState, _) -> ({ model | navbar = newState }, Cmd.none)

    (_, _) -> ({ model | content = Invalid model msg }, Cmd.none)

-- Cool story here, I was trying to parse the url, and was using Url.fromString path.
-- The problem is that it always returns Nothing, because the expected url needs to
-- be an absolute url *sadface*
validLinks : Model -> String -> Maybe (Model, Cmd Msg)
validLinks model path =
  let endpointToInit : Endpoint -> (Model, Cmd Msg)
      endpointToInit ep = case ep of
        LandingPage -> Landing.init |> liftModelCmd Landing LandingMsg model
        ArticlesPage -> Articles.init |> liftModelCmd Articles ArticlesMsg model
        VideosPage -> VideoList.init |> liftModelCmd VideoList VideoListMsg model
        VideoAddPage -> VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model
        Cokk2020Page -> Cokkolo2020.Landing.init |> liftModelCmd CokkoloLanding2020 CokkoloLanding2020Msg model
        Cokk2020ResultsPage -> Cokkolo2020.Results.init |> liftModelCmd CokkoloResults2020 CokkoloResults2020Msg model
        Cokk2021Page -> Cokkolo2021.Landing.init |> liftModelCmd CokkoloLanding2021 CokkoloLanding2021Msg model
        Cokk2021ResultsPage -> Cokkolo2021.Results.init |> liftModelCmd CokkoloResults2021 CokkoloResults2021Msg model
        BlogListPage -> BlogList.init |> liftModelCmd BlogList BlogListMsg model
        BlogItemPage index -> Blog.init index |> liftModelCmd Blog BlogMsg model
        FilmsPage -> Films.init |> liftModelCmd Films FilmsMsg model
  in path
     |> Endpoints.parse
     |> Maybe.map endpointToInit

selectPage : Model -> String -> (Model, Cmd Msg)
selectPage model path =
  let loading = { model | content = Loading }
  in
    validLinks model path
    |> Maybe.map (\_ -> pushNewUrl model.key path (loading, Cmd.none))
    |> Maybe.withDefault (loading, Nav.load path)

view : Model -> Document Msg
view model =
  case model.content of
    Landing welcome -> Landing.view welcome |> liftDocument model LandingMsg
    CokkoloLanding2020 cl -> Cokkolo2020.Landing.view cl |> liftDocument model CokkoloLanding2020Msg
    CokkoloResults2020 cr -> Cokkolo2020.Results.view cr |> liftDocument model CokkoloResults2020Msg
    CokkoloLanding2021 cl -> Cokkolo2021.Landing.view cl |> liftDocument model CokkoloLanding2021Msg
    CokkoloResults2021 cr -> Cokkolo2021.Results.view cr |> liftDocument model CokkoloResults2021Msg
    Articles articles -> Articles.view articles |> liftDocument model ArticlesMsg
    VideoAdd videoAdd -> VideoAdd.view videoAdd |> liftDocument model VideoAddMsg
    VideoList videoList -> VideoList.view videoList |> liftDocument model VideoListMsg
    BlogList blog -> BlogList.view blog |> liftDocument model BlogListMsg
    Blog blog -> Blog.view blog |> liftDocument model BlogMsg
    Films films -> Films.view films |> liftDocument model FilmsMsg
    Loading -> { title = "Loading", body = [text "If you see this just hit refresh :D #developer"] }
    Test msg -> { title = "Test", body = [text msg] }
    Invalid _ _ -> { title = "Error", body = [text "mismatch"] }

navbar : Model -> Html Msg
navbar model =
  Navbar.config NavbarMsg
  |> Navbar.withAnimation
  |> Navbar.brand [ href <| Endpoints.show LandingPage ] [ text "Gyurver"]
  |> Navbar.items
    [ Navbar.itemLink [ href <| Endpoints.show ArticlesPage ] [ text "📑 Articles"]
    , Navbar.itemLink [ href <| Endpoints.show BlogListPage ] [ text "📝 Blog" ]
    , Navbar.itemLink [ href <| Endpoints.show VideosPage ] [ text "📼 Videos"]
    , Navbar.itemLink [ href <| Endpoints.show FilmsPage ] [ text "🎞️ Films"]
    , Navbar.dropdown
      { id = "Cokkolo_Dropdown"
      , toggle = Navbar.dropdownToggle [] [text "🥚 Cökkölő"]
      , items =
        [ Navbar.dropdownItem [href <| Endpoints.show Cokk2020Page] [text "2020"]
        , Navbar.dropdownItem [href <| Endpoints.show Cokk2021Page] [text "2021"]
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
liftModelCmd fm fc model (m, cmd) = ({ model | content = fm m }, Cmd.map fc cmd)

pushNewUrl : Key -> String -> (Model, Cmd Msg) -> (Model, Cmd Msg)
pushNewUrl key path (model, cmd) = (model, Cmd.batch [Nav.pushUrl key path, cmd])
