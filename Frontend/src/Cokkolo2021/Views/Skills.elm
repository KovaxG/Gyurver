module Cokkolo2021.Views.Skills exposing (..)

import Html exposing (Html, text, h2)
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Table as Table

import Cokkolo2021.Common exposing (..)

type alias ViewState = { user : User }

init : User -> ViewState
init user = { user = user }

type Message = SwitchToDashboard

view : ViewState -> Html Message
view state =
  let
    tableRow : String -> (Skills -> Int) -> Table.Row Message
    tableRow name f =
      let level = f state.user.skills
      in
        [ Table.td [] [text name]
        , Table.td [] [text <| String.fromInt level ++ "/10"]
        , Table.td [] [text <| String.fromInt (level + 1) ++ " 💦"]
        , Table.td [] [Button.button [Button.outlineSuccess] [text "➕"]]
        ] |> Table.tr []
  in
    [ [ h2 [] [text "Képességek"]
      , Button.button
        [ Button.outlineSecondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick SwitchToDashboard
        ] [text "Vissza"]
      , Table.table
        { options = [ Table.striped ]
        , thead =
            Table.simpleThead
                [ Table.th [] [text "Elnevezes"]
                , Table.th [] [text "Pontok"]
                , Table.th [] [text "Ár"]
                , Table.th [] []
                ]
        , tbody =
          [ tableRow "keménység" .kemenyseg
          , tableRow "erősség" .erosseg
          , tableRow "settenkedés" .settenkedes
          , tableRow "szivarozás" .szivarozas
          , tableRow "furfangosság" .furfangossag
          , tableRow "tűzokádás" .tuzokadas
          , tableRow "zsírosság" .zsirossag
          , tableRow "intelligencia" .intelligencia
          , tableRow "diplomácia" .diplomacia
          , tableRow "hegyesség" .hegyesseg
          , tableRow "szerencse" .szerencse
          , tableRow "bájosság" .baj
          , tableRow "meggyőzőerő" .meggyozoero
          , tableRow "precízitás" .precizitas
          , tableRow "nyelvtudás" .nyelvtudas
          , tableRow "ízlés" .izles
          , tableRow "vérnyomás" .vernyomas
          , tableRow "humorérzék" .humorerzek
          , tableRow "regeneráció" .regeneracio
          , tableRow "művészlélek" .muveszlelek
          , tableRow "tisztaságmánia" .tisztasagmania
          , tableRow "edzettség" .edzettseg
          ]
          |> Table.tbody []
        }
      ] |> Grid.col []
    ] |> Grid.row []
