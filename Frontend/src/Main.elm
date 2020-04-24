module Main exposing (..)

import Browser exposing (UrlRequest(..), Document, application)
import Browser.Navigation exposing (Key)
import Browser.Navigation as Nav
import Html exposing (Html, text)
import Url exposing (Url)
import Debug

import Landing
import CokkList
import Eredmenyek
import Articles

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

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
  
init : () -> Url -> Key -> (Model, Cmd Msg)
init flags url key = 
  selectPage { content = Loading, key = key } url.path
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  let invalidState = ({ model | content = Invalid model msg }, Cmd.none)
  in case msg of
    LandingMsg welcomeMsg -> 
      case model.content of
        Landing welcome -> 
          Landing.update welcomeMsg welcome
          |> liftModelCmd Landing LandingMsg model
        _ ->  invalidState
    CokkListMsg cokkListMsg ->
      case model.content of
        CokkList cokkList ->
          CokkList.update cokkListMsg cokkList
          |> liftModelCmd CokkList CokkListMsg model
        _ ->  invalidState
    EredmenyekMsg eredmenyekMsg ->
      case model.content of
        Eredmenyek eredmenyek ->
          Eredmenyek.update eredmenyekMsg eredmenyek
          |> liftModelCmd Eredmenyek EredmenyekMsg model
        _ -> invalidState
    ArticlesMsg articlesMsg ->
      case model.content of
        Articles articles ->
          Articles.update articlesMsg articles
          |> liftModelCmd Articles ArticlesMsg model
        _ -> invalidState
    UrlRequest request -> 
      case request of
        External path -> ({ model | content = Loading }, Nav.load path)
        Internal url -> selectPage model url.path
    UrlChange url -> 
      case url.path of
        "/" ->
          Landing.init
          |> liftModelCmd Landing LandingMsg model
        "/cokk" ->  
          CokkList.init
          |> liftModelCmd CokkList CokkListMsg model
        "/cokk/eredmeny" -> 
          Eredmenyek.init
          |> liftModelCmd Eredmenyek EredmenyekMsg  model
        "/articles" ->
          Articles.init
          |> liftModelCmd Articles ArticlesMsg  model
        _ -> ({ model | content = Loading }, Cmd.none)

selectPage : Model -> String -> (Model, Cmd Msg)
selectPage model path = 
  let
    loading = { model | content = Loading }
    pushUrl = pushNewUrl model.key path
  in case path of
    "/" -> pushUrl (loading, Cmd.none) 
    "/cokk" -> pushUrl (loading, Cmd.none) 
    "/cokk/eredmeny" -> pushUrl (loading, Cmd.none)
    "/articles" -> pushUrl (loading, Cmd.none)
    _ -> (loading, Nav.load path)
    
view : Model -> Document Msg
view model =
  case model.content of
    Landing welcome -> 
      Landing.view welcome 
      |> liftDocument LandingMsg
    CokkList cokkList ->
      CokkList.view cokkList
      |> liftDocument CokkListMsg
    Eredmenyek eredmenyek ->
      Eredmenyek.view eredmenyek
      |> liftDocument EredmenyekMsg
    Articles articles ->
      Articles.view articles
      |> liftDocument ArticlesMsg
    Invalid md ms -> 
      { title = "Error"
      , body = 
        [text <| Debug.toString md ++ " - " ++ Debug.toString ms] 
      } 
    Loading -> { title = "Loading", body = [text "Loading..."]}
    Test msg -> { title = "Test", body = [text msg] }
    
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
