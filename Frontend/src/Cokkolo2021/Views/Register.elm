module Cokkolo2021.Views.Register exposing (..)

import Html exposing (Html, text, h1, h2)
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as Encode exposing (Value)

import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { username : String
  , password1 : String
  , password2 : String
  , eggname : String
  , state : ViewStatus
  }

encode : ViewState -> Value
encode s =
  Encode.object
    [ ("username", Encode.string s.username)
    , ("password", Encode.string s.password1)
    , ("eggname", Encode.string s.eggname)
    ]

init : ViewState
init =
  { username = ""
  , password1 = ""
  , password2 = ""
  , eggname = ""
  , state = Normal
  }

type Message
  = UsernameFieldChange String
  | Password1FieldChange String
  | Password2FieldChange String
  | EggnameFieldChange String
  | SwitchToLoginView
  | Register

isValidName : String -> Bool
isValidName = not << String.isEmpty << String.trim

view : ViewState -> Html Message
view state =
  [ [ h1 [] [text "2021 Húsvéti játékok"]
      , h2 [] [text "Regisztrálás"]
      , text "Gazda neve"
      , Input.text
      [ Input.value state.username
      , Input.onInput UsernameFieldChange
      , if isValidName state.username then Input.success else Input.danger
      ]
      , text "Tojás Neve"
      , Input.text
      [ Input.value state.eggname
      , Input.onInput EggnameFieldChange
      , if isValidName state.eggname then Input.success else Input.danger
      ]
      , text "Jelszó"
      , Input.text
      [ Input.value state.password1
      , Input.onInput Password1FieldChange
      ]
      , text "Jelszó de mégegyszer"
      , Input.text
      [ Input.value state.password2
      , Input.onInput Password2FieldChange
      , if state.password1 /= state.password2 || String.isEmpty state.password1 then Input.danger else Input.success
      ]
      , Button.button
      [ Button.primary
      , Button.disabled (state.password1 /= state.password2 || not (isValidName state.username) || not (isValidName state.eggname) || String.isEmpty state.password1)
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick Register
      ] [text "Regisztrálás!"]
      , Button.button
      [ Button.secondary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToLoginView
      ] [text "Vissza"]
      , case state.state of
          Problem str -> text str
          Waiting -> text "waiting..."
          Normal -> text ""
      ] |> Grid.col []
  ] |> Grid.row []
