module Cokkolo2021.Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h2, h3, p, ol, li, br, a)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing

type alias LoginViewState =
  { username : String
  , password : String
  }

loginViewInitState : LoginViewState
loginViewInitState =
  { username = ""
  , password = ""
  }

type alias RegisterViewState =
  { username : String
  , password1 : String
  , password2 : String
  , eggname : String
  }

registerViewInitState : RegisterViewState
registerViewInitState =
  { username = ""
  , password1 = ""
  , password2 = ""
  , eggname = ""
  }

type alias DashboardViewState = {}

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
  | RegisterViewUsernameFieldChange String
  | RegisterViewPassword1FieldChange String
  | RegisterViewPassword2FieldChange String
  | RegisterViewEggnameFieldChange String
  | RegisterViewSwitchToLoginView

init : (Model, Cmd Msg)
init = (LoginView loginViewInitState, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
  (LoginViewUsernameFieldChange d, LoginView s) -> (LoginView { s | username = d }, Cmd.none)
  (LoginViewPasswordFieldChange d, LoginView s) -> (LoginView { s | password = d }, Cmd.none)
  (LoginViewSwitchToRegisterView, LoginView s) -> (RegisterView registerViewInitState, Cmd.none)
  (RegisterViewUsernameFieldChange d, RegisterView s) -> (RegisterView { s | username = d }, Cmd.none)
  (RegisterViewPassword1FieldChange d, RegisterView s) -> (RegisterView { s | password1 = d }, Cmd.none)
  (RegisterViewPassword2FieldChange d, RegisterView s) -> (RegisterView { s | password2 = d }, Cmd.none)
  (RegisterViewEggnameFieldChange d, RegisterView s) -> (RegisterView { s | eggname = d }, Cmd.none)
  (RegisterViewSwitchToLoginView, RegisterView s) -> (LoginView loginViewInitState, Cmd.none)
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
        , Button.onClick LoginViewLogin
        ] [text "Engeddj be!"]
      , Button.button
        [ Button.secondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick RegisterViewSwitchToLoginView
        ] [text "Vissza"]
      ] |> Grid.col []
    ] |> Grid.row []
  _ -> div [] [text "Not implemented!"]
