module Cokkolo2021.Views.Contestants exposing (..)

import Html exposing (Html, text, h1, div, br)
import Html.Events exposing (onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Table as Table
import List.Extra as List

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

import Cokkolo2021.Types.Fight as Fight
import Cokkolo2021.Common exposing (..)
import Settings

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
  , ontoztek : Int
  , ontozott : Int
  }

userToContestant : User -> List Contestant -> Contestant
userToContestant user cs =
  cs
  |> List.find (\c -> c.username == user.username)
  |> Maybe.withDefault
    { username = user.username
    , eggname = user.eggName
    , base = user.base
    , waterable = True
    , skills = user.skills
    , ontoztek = -1
    , ontozott = -1
    }

decode : Decoder Contestant
decode =
  Decode.map7
    Contestant
    (Decode.field "username" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "base" itemDecoder)
    (Decode.field "waterable" Decode.bool)
    (Decode.field "skills" skillsDecoder)
    (Decode.field "ontoztek" Decode.int)
    (Decode.field "ontozott" Decode.int)

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
  | FightRequest User Contestant
  | FightRequestFailure String
  | FightRequestSuccess Contestant (List Fight.FightLog)

view : ViewState -> Html Message
view state =
  [ [ h1 [] [text "Résztvevők"]
      , description
      , Button.button
      [ Button.outlineSecondary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToDashboardView
      ] [text "Vissza"]
      , Table.table
      { options = [ Table.hover ]
      , thead =
          Table.simpleThead
              [ Table.th [] []
              , Table.th [] []
              , Table.th [] [text "Tojás"]
              , Table.th [] [text "Gazda"]
              , Table.th [] []
              , Table.th [] []
              ]
      , tbody =
        state.items
        |> List.filterNot (\contestant -> contestant.username == state.user.username)
        |> (\list -> userToContestant state.user state.items :: list)
        |> List.indexedMap (\index contestant ->
            [ Table.td [Table.cellAttr (onClick <| SwitchToEggView contestant)] [ text <| String.fromInt (index + 1) ]
            , Table.td [Table.cellAttr (onClick <| SwitchToEggView contestant)] [ displayImage contestant.base.image 61 82 ]
            , Table.td [Table.cellAttr (onClick <| SwitchToEggView contestant)] [ text contestant.eggname ]
            , Table.td [Table.cellAttr (onClick <| SwitchToEggView contestant)] [ text contestant.username ]
            , Table.td [] [ if contestant.username == state.user.username
                            then text "(te vagy)"
                            else if contestant.waterable
                            then Button.button
                              [ Button.outlineSecondary
                              , Button.onClick (WaterUser contestant.username)
                              ] [text "💦"]
                            else text "(ma már megöntözted)"
                        ]
            , Table.td []
              [Button.button
                [ Button.outlineSecondary
                , Button.onClick (FightRequest state.user contestant)
                ] [text "🤜🤛"]]
            ] |> Table.tr (if not contestant.waterable then [Table.rowSuccess] else [])
        )
        |> Table.tbody []
      }
      ] |> Grid.col []
  ] |> Grid.row []

description : Html Message
description =
  [ text "Itt van az összes versenyző, egyszer egy nap megöntözheted őket (ami neked nem kerül semmibe) és ők kapnak egy kölnit. Persze ha mások téged megöntöznek akkor te kapsz egy kölnit."
  , br [] []
  , br [] []
  , text "Ráklikkelhetsz egy tojásra, hogy lássad milyen felszerelése és milyen tulajdonságai vannak annak a tojásnak. Azt is látod, hogy hányan öntözték meg azt a gazdit, és hányat öntözött meg az a gazdi."
  , br [] []
  , br [] []
  , text "Kölcsön kenyér vissza jár, tehát azt javasolom, hogy ne spórolj az öntözéssel :)"
  , br [] []
  ] |> div []
