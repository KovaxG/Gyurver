module Cokkolo2021.Views.Store exposing (..)

import Html exposing (Html, text, h2)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { user : User }

type Message
  = SwitchToDashboard

init : User -> ViewState
init user = { user = user }

view : ViewState -> Html Message
view state =
  [ [ h2 [] [text "Üzlet"]
      , Button.button
        [ Button.outlineSecondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick SwitchToDashboard
        ] [text "Vissza"]
      ] |> Grid.col []
  ] |> Grid.row []
