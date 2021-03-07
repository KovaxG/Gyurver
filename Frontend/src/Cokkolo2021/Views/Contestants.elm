module Cokkolo2021.Views.Contestants exposing (..)

import Html exposing (Html, text, h1)
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Table as Table

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { user : User
  , items : List Contestant
  }

type alias Contestant =
  { username : String
  , eggname : String
  , image : String
  , waterable : Bool
  }

decode : Decoder Contestant
decode =
  Decode.map4
    Contestant
    (Decode.field "username" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "image" Decode.string)
    (Decode.field "waterable" Decode.bool)

init : User -> ViewState
init u =
  { user = u
  , items = []
  }

encodeWaterBody : String -> User -> Value
encodeWaterBody target user =
  Encode.object
    [ ("username", Encode.string user.username)
    , ("target", Encode.string target)
    , ("password", Encode.string user.password)
    ]

type Message
  = SwitchToDashboardView
  | PopulateList (List Contestant)
  | WaterUser String
  | WateringSuccess

view : ViewState -> Html Message
view state =
  [ [ h1 [] [text "Résztvevők"]
      , Button.button
      [ Button.primary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToDashboardView
      ] [text "Vissza"]
      , Table.table
      { options = []
      , thead =
          Table.simpleThead
              [ Table.th [] []
              , Table.th [] [text "Tojás"]
              , Table.th [] [text "Gazda"]
              , Table.th [] []
              ]
      , tbody =
              state.items
              |> List.map (\c ->
                  [ Table.td [] [ displayImage c.image 50 50 ]
                  , Table.td [] [ text c.eggname ]
                  , Table.td [] [ text c.username ]
                  , Table.td [] [ if c.username == state.user.username then text "(te vagy)"
                                  else if c.waterable then Button.button [ Button.outlineSecondary, Button.onClick (WaterUser c.username) ] [text "💦"]
                                  else text "(ma mar megontozted)"
                              ]
                  ] |> Table.tr (if not c.waterable then [Table.rowSuccess] else [])
              )
              |> Table.tbody []
      }
      ] |> Grid.col []
  ] |> Grid.row []
