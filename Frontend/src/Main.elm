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

main : Program () Model Msg
main = application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = UrlRequest
  , onUrlChange = UrlChange
  }

type Model
  = Landing Landing.Model
  | CokkList CokkList.Model
  | Eredmenyek Eredmenyek.Model
  | Invalid Model Msg
  | Loading

type Msg
  = UrlRequest UrlRequest
  | UrlChange Url
  | LandingMsg Landing.Msg
  | CokkListMsg CokkList.Msg
  | EredmenyekMsg Eredmenyek.Msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
  
init : () -> Url -> Key -> (Model, Cmd Msg)
init flags url key = 
  Landing.init
  |> liftModelCmd Landing LandingMsg 
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    LandingMsg welcomeMsg -> 
      case model of
        Landing welcome -> 
          Landing.update welcomeMsg welcome
          |> liftModelCmd Landing LandingMsg
        _ ->  (Invalid model msg, Cmd.none)
    CokkListMsg cokkListMsg ->
      case model of
        CokkList cokkList ->
          CokkList.update cokkListMsg cokkList
          |> liftModelCmd CokkList CokkListMsg
        _ ->  (Invalid model msg, Cmd.none)
    EredmenyekMsg eredmenyekMsg ->
      case model of
        Eredmenyek eredmenyek ->
          Eredmenyek.update eredmenyekMsg eredmenyek
          |> liftModelCmd Eredmenyek EredmenyekMsg
        _ -> (Invalid model msg, Cmd.none)
    UrlRequest request -> 
      case request of
        External path -> (Loading, Nav.load path)
        Internal url -> selectPage url.path
          
    _ -> (Invalid model msg, Cmd.none)

selectPage : String -> (Model, Cmd Msg)
selectPage path = 
  case path of
    "/cokk" ->  liftModelCmd CokkList CokkListMsg CokkList.init
    "/cokk/eredmeny" -> liftModelCmd Eredmenyek EredmenyekMsg Eredmenyek.init
    "/cv" -> (Loading, Nav.load path)
    _ -> (Loading, Nav.load path)
    

view : Model -> Document Msg
view model =
  case model of
    Landing welcome -> 
      Landing.view welcome 
      |> liftDocument LandingMsg
    CokkList cokkList ->
      CokkList.view cokkList
      |> liftDocument CokkListMsg
    Eredmenyek eredmenyek ->
      Eredmenyek.view eredmenyek
      |> liftDocument EredmenyekMsg
    Invalid md ms -> { title = "Error", body = [text <| Debug.toString md ++ " - " ++ Debug.toString ms] } 
    Loading -> { title = "Loading", body = [text "Loading..."]}
    
liftDocument : (a -> b) -> Document a -> Document b
liftDocument f da = 
  { title = da.title
  , body = List.map (Html.map f) da.body
  }
  
liftModelCmd : (a -> c) -> (b -> d) -> (a, Cmd b) -> (c, Cmd d)
liftModelCmd fm fc (model, cmd) = (fm model, Cmd.map fc cmd)
