module Cokkolo2021.Views.Skills exposing (..)

import Html exposing (Html, text, h2, div, br)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)

import Cokkolo2021.Common exposing (..)

type alias ViewState = { user : User }

init : User -> ViewState
init user = { user = user }

update : String -> Int -> ViewState -> ViewState
update skill cost state =
  let user = state.user
  in { state | user = { user | perfume = user.perfume - cost, skills = Maybe.withDefault user.skills <| modifySkill skill (\s -> s + 1) user.skills } }

toIncSkillRequest : ViewState -> String -> Value
toIncSkillRequest state skill = Encode.object
  [ ("username", Encode.string state.user.username)
  , ("password", Encode.string state.user.password)
  , ("skill", Encode.string skill)
  ]

type Message
  = SwitchToDashboard
  | IncSkill String Int
  | IncSkillSuccess String Int
  | IncSkillFailure String

view : ViewState -> Html Message
view state =
  let
    row : String -> (Skills -> Int) -> String -> Table.Row Message
    row displayName f name =
      let level = f state.user.skills
          levelCost = level + 1
          tooExpensive = levelCost > state.user.perfume
          buttonStyle = if tooExpensive then Button.outlineSecondary else Button.outlineSuccess
          (cost, button) =
            if level >= 10
            then ("", [])
            else ( String.fromInt levelCost ++ " 💦"
                 , [Button.button [buttonStyle, Button.onClick <| IncSkill name levelCost, Button.disabled tooExpensive] [text "➕"]]
                 )
      in
        [ Table.td [] [text displayName]
        , Table.td [] [text <| String.fromInt level ++ "/10"]
        , Table.td [] [text cost]
        , Table.td [] button
        ] |> Table.tr []
  in
    [ [ h2 [] [text "A tojásod tulajdonságai"]
      , description
      , Button.button
        [ Button.outlineSecondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick SwitchToDashboard
        ] [text "Vissza"]
      , h2 [] [text <| "Kölni: " ++ String.fromInt state.user.perfume ++ " 💦"]
      , Table.table
        { options = [ Table.striped ]
        , thead =
            Table.simpleThead
                [ Table.th [] [text "Elnevezés"]
                , Table.th [] [text "Pontok"]
                , Table.th [] [text "Ár"]
                , Table.th [] []
                ]
        , tbody =
          [ row "keménység" .kemenyseg "kemenyseg"
          , row "erősség" .erosseg "erosseg"
          , row "settenkedés" .settenkedes "settenkedes"
          , row "szivarozás" .szivarozas "szivarozas"
          , row "furfangosság" .furfangossag "furfangossag"
          , row "tűzokádás" .tuzokadas "tuzokadas"
          , row "zsírosság" .zsirossag "zsirossag"
          , row "intelligencia" .intelligencia "intelligencia"
          , row "diplomácia" .diplomacia "diplomacia"
          , row "hegyesség" .hegyesseg "hegyesseg"
          , row "szerencse" .szerencse "szerencse"
          , row "bájosság" .baj "baj"
          , row "meggyőzőerő" .meggyozoero "meggyozoero"
          , row "precízitás" .precizitas "precizitas"
          , row "nyelvtudás" .nyelvtudas "nyelvtudas"
          , row "ízlés" .izles "izles"
          , row "vérnyomás" .vernyomas "vernyomas"
          , row "humorérzék" .humorerzek "humorerzek"
          , row "regeneráció" .regeneracio "regeneracio"
          , row "művészlélek" .muveszlelek "muveszlelek"
          , row "tisztaságmánia" .tisztasagmania "tisztasagmania"
          , row "edzettség" .edzettseg "edzettseg"
          ]
          |> Table.tbody []
        }
      ] |> Grid.col []
    ] |> Grid.row []

description : Html Message
description =
  [ text "A bajnokság napján a tojásod tulajdonságaik döntik el, ha nyer vagy veszít egy meccset. Persze a tojás keménysége nem minden, ezért nagyon sokfajta megközelítéssel lehet elönyt szerezni."
  , br [] []
  ] |> div []
