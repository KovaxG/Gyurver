module Cokkolo2021.Views.Contestants exposing (..)

import Html exposing (Html, text, h1)
import Html.Events exposing (onClick)
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
  , base : Item
  , waterable : Bool
  , skills : Skills
  }

decode : Decoder Contestant
decode =
  Decode.map5
    Contestant
    (Decode.field "username" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "base" itemDecoder)
    (Decode.field "waterable" Decode.bool)
    (Decode.field "skills" skillsDecoder)

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
  | SwitchToEggView Contestant
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
      { options = [ Table.hover ]
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
            [ Table.td [Table.cellAttr (onClick <| SwitchToEggView c)] [ displayImage c.base.image 50 50 ]
            , Table.td [Table.cellAttr (onClick <| SwitchToEggView c)] [ text c.eggname ]
            , Table.td [Table.cellAttr (onClick <| SwitchToEggView c)] [ text c.username ]
            , Table.td [] [ if c.username == state.user.username
                            then text "(te vagy)"
                            else if c.waterable
                            then Button.button
                              [ Button.outlineSecondary
                              , Button.onClick (WaterUser c.username)
                              ] [text "💦"]
                            else text "(ma már megöntözted)"
                        ]
            ] |> Table.tr (if not c.waterable then [Table.rowSuccess] else [])
        )
        |> Table.tbody []
      }
      ] |> Grid.col []
  ] |> Grid.row []
