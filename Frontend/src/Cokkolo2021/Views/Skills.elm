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
    row : String -> (Skills -> Int) -> Table.Row Message
    row name f =
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
          [ row "keménység" .kemenyseg
          , row "erősség" .erosseg
          , row "settenkedés" .settenkedes
          , row "szivarozás" .szivarozas
          , row "furfangosság" .furfangossag
          , row "tűzokádás" .tuzokadas
          , row "zsírosság" .zsirossag
          , row "intelligencia" .intelligencia
          , row "diplomácia" .diplomacia
          , row "hegyesség" .hegyesseg
          , row "szerencse" .szerencse
          , row "bájosság" .baj
          , row "meggyőzőerő" .meggyozoero
          , row "precízitás" .precizitas
          , row "nyelvtudás" .nyelvtudas
          , row "ízlés" .izles
          , row "vérnyomás" .vernyomas
          , row "humorérzék" .humorerzek
          , row "regeneráció" .regeneracio
          , row "művészlélek" .muveszlelek
          , row "tisztaságmánia" .tisztasagmania
          , row "edzettség" .edzettseg
          ]
          |> Table.tbody []
        }
      ] |> Grid.col []
    ] |> Grid.row []
