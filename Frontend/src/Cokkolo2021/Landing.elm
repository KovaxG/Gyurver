module Cokkolo2021.Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h2, h3, p, ol, li, br, a)
import Http as Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Debug

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing

import Settings
import Endpoints
import Util

type ViewState = Normal | Waiting | Problem String

type alias LoginViewState =
  { username : String
  , password : String
  , state : ViewState
  }

encodeLoginViewState : LoginViewState -> Value
encodeLoginViewState s =
  Encode.object
    [ ("user", Encode.string s.username)
    , ("pass", Encode.string s.password)
    ]

loginViewInitState : LoginViewState
loginViewInitState =
  { username = ""
  , password = ""
  , state = Normal
  }

type alias RegisterViewState =
  { username : String
  , password1 : String
  , password2 : String
  , eggname : String
  , state : ViewState
  }

encodeRegisterViewState : RegisterViewState -> Value
encodeRegisterViewState s =
  Encode.object
    [ ("username", Encode.string s.username)
    , ("password", Encode.string s.password1)
    , ("eggname", Encode.string s.eggname)
    ]

registerViewInitState : RegisterViewState
registerViewInitState =
  { username = ""
  , password1 = ""
  , password2 = ""
  , eggname = ""
  , state = Normal
  }


type alias User =
  { username : String
  , password : String
  , eggName : String
  }

userDecoder : Decoder User
userDecoder =
  Decode.map3
    User
    (Decode.field "username" Decode.string)
    (Decode.field "password" Decode.string)
    (Decode.field "eggname" Decode.string)

type alias DashboardViewState = User

populateDashBoardState : User -> DashboardViewState
populateDashBoardState user = user

type View
  = LoginView LoginViewState
  | RegisterView RegisterViewState
  | DashboardView DashboardViewState

type alias Model = View
type Msg
  = LoginViewUsernameFieldChange String
  | LoginViewPasswordFieldChange String
  | LoginViewLogin
  | LoginViewSwitchToRegisterView
  | LoginViewLoginSuccess User
  | LoginViewLoginFailure String
  | RegisterViewUsernameFieldChange String
  | RegisterViewPassword1FieldChange String
  | RegisterViewPassword2FieldChange String
  | RegisterViewEggnameFieldChange String
  | RegisterViewSwitchToLoginView
  | RegisterViewRegister
  | RegisterViewRegisterSuccess User
  | RegisterViewRegisterFailure String

init : (Model, Cmd Msg)
init = (LoginView loginViewInitState, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
  (LoginViewUsernameFieldChange d, LoginView s) -> (LoginView { s | username = d }, Cmd.none)
  (LoginViewPasswordFieldChange d, LoginView s) -> (LoginView { s | password = d }, Cmd.none)
  (LoginViewSwitchToRegisterView, LoginView s) -> (RegisterView registerViewInitState, Cmd.none)
  (LoginViewLoginFailure err, LoginView s) -> (LoginView { s | state = Problem err }, Cmd.none)
  (LoginViewLoginSuccess user, LoginView s) -> (DashboardView <| populateDashBoardState user, Cmd.none)
  (LoginViewLogin, LoginView s) ->
    ( LoginView { s | state = Waiting }
    , Http.post
        { url = Settings.path ++ Endpoints.cokk2021LoginJson
        , body = Http.jsonBody (encodeLoginViewState s)
        , expect = Http.expectJson (Util.processMessage LoginViewLoginSuccess LoginViewLoginFailure) userDecoder
        }
    )
  (RegisterViewUsernameFieldChange d, RegisterView s) -> (RegisterView { s | username = d }, Cmd.none)
  (RegisterViewPassword1FieldChange d, RegisterView s) -> (RegisterView { s | password1 = d }, Cmd.none)
  (RegisterViewPassword2FieldChange d, RegisterView s) -> (RegisterView { s | password2 = d }, Cmd.none)
  (RegisterViewEggnameFieldChange d, RegisterView s) -> (RegisterView { s | eggname = d }, Cmd.none)
  (RegisterViewSwitchToLoginView, RegisterView s) -> (LoginView loginViewInitState, Cmd.none)
  (RegisterViewRegisterSuccess user, RegisterView s) -> (DashboardView <| populateDashBoardState user, Cmd.none)
  (RegisterViewRegisterFailure err, RegisterView s) -> (RegisterView { s | state = Problem err }, Cmd.none)
  (RegisterViewRegister, RegisterView s) ->
    ( RegisterView { s | state = Waiting }
    , Http.post
        { url = Settings.path ++ Endpoints.cokk2021RegisterJson
        , body = Http.jsonBody (encodeRegisterViewState s)
        , expect = Http.expectJson (Util.processMessage RegisterViewRegisterSuccess RegisterViewRegisterFailure) userDecoder
        }
    )
  _ -> (LoginView loginViewInitState, Cmd.none)

view : Model -> Document Msg
view model =
  { title = "Cokkolo 2021"
  , body =
    [ [ CDN.stylesheet
      , showPage model
      ] |> Grid.container []
    ]
  }

showPage : View -> Html Msg
showPage v = case v of
  LoginView state ->
    [ [ h1 [] [text "2021 Húsvéti játékok"]
      , text "Itt kene legyen a leiras"
      , h2 [] [text "Belépés"]
      , text "Gazda"
      , Input.text [Input.value state.username, Input.onInput LoginViewUsernameFieldChange]
      , text "Jelszó"
      , Input.password [Input.value state.password, Input.onInput LoginViewPasswordFieldChange]
      , Button.button
        [ Button.primary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick LoginViewLogin
        ] [text "Engeddj be!"]
      , Button.button
        [ Button.secondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick LoginViewSwitchToRegisterView
        ] [text "Én is akarok tojást!"]
      , case state.state of
          Problem str -> text str
          Waiting -> text "waiting..."
          Normal -> text ""
      ] |> Grid.col []
    ] |> Grid.row []
  RegisterView state ->
    [ [ h1 [] [text "2021 Húsvéti játékok"]
      , h2 [] [text "Regisztrálás"]
      , text "Gazda neve"
      , Input.text [Input.value state.username, Input.onInput RegisterViewUsernameFieldChange]
      , text "Jelszó"
      , Input.text
        [ Input.value state.password1
        , Input.onInput RegisterViewPassword1FieldChange
        ]
      , text "Jelszó de mégegyszer"
      , Input.text
        [ Input.value state.password2
        , Input.onInput RegisterViewPassword2FieldChange
        , if state.password1 /= state.password2 || state.password1 == "" then Input.danger else Input.success
        ]
      , text "Tojás Neve"
      , Input.text [Input.value state.eggname, Input.onInput RegisterViewEggnameFieldChange]
      , Button.button
        [ Button.primary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick RegisterViewRegister
        ] [text "Regisztrálás!"]
      , Button.button
        [ Button.secondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick RegisterViewSwitchToLoginView
        ] [text "Vissza"]
      , case state.state of
           Problem str -> text str
           Waiting -> text "waiting..."
           Normal -> text ""
      ] |> Grid.col []
    ] |> Grid.row []
  DashboardView state ->
    [ [ h1 [] [text "Dashboard"]
      , text <| Debug.toString state
      ] |> Grid.col []
    ] |> Grid.row []
