module Cokkolo2021.Views.Login exposing (..)

import Html exposing (Html, text, h1, h2)
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as Encode exposing (Value)

import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { username : String
  , password : String
  , state : ViewStatus
  }

init : ViewState
init =
  { username = ""
  , password = ""
  , state = Normal
  }

encodeGeneric : String -> String -> Value
encodeGeneric username password =
  Encode.object
    [ ("user", Encode.string username)
    , ("pass", Encode.string password)
    ]

encode : ViewState -> Value
encode state = encodeGeneric state.username (encryptPass state.password)

type Message
  = UsernameFieldChange String
  | PasswordFieldChange String
  | Login
  | SwitchToRegisterView

view : ViewState -> Html Message
view state =
  [ [ h1 [] [text "2021 Húsvéti játékok"]
    , text "Itt kene legyen a leiras"
    , h2 [] [text "Belépés"]
    , text "Gazda"
    , Input.text [Input.value state.username, Input.onInput UsernameFieldChange]
    , text "Jelszó"
    , Input.password [Input.value state.password, Input.onInput PasswordFieldChange]
    , Button.button
    [ Button.primary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick Login
    ] [text "Engeddj be!"]
    , Button.button
    [ Button.secondary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick SwitchToRegisterView
    ] [text "Én is akarok tojást!"]
    , case state.state of
        Problem str -> text str
        Waiting -> text "waiting..."
        Normal -> text ""
    ] |> Grid.col []
  ] |> Grid.row []
