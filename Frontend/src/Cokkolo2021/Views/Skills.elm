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
              ]
      , tbody =
        [ [ Table.td [] [text "keménység"], Table.td [] [text <| String.fromInt state.user.skills.kemenyseg] ] |> Table.tr []
        , [ Table.td [] [text "erősség"], Table.td [] [text <| String.fromInt state.user.skills.erosseg] ] |> Table.tr []
        , [ Table.td [] [text "settenkedés"], Table.td [] [text <| String.fromInt state.user.skills.settenkedes] ] |> Table.tr []
        , [ Table.td [] [text "szivarozás"], Table.td [] [text <| String.fromInt state.user.skills.szivarozas] ] |> Table.tr []
        , [ Table.td [] [text "furfangosság"], Table.td [] [text <| String.fromInt state.user.skills.furfangossag] ] |> Table.tr []
        , [ Table.td [] [text "tűzokádás"], Table.td [] [text <| String.fromInt state.user.skills.tuzokadas] ] |> Table.tr []
        , [ Table.td [] [text "zsírosság"], Table.td [] [text <| String.fromInt state.user.skills.zsirossag] ] |> Table.tr []
        , [ Table.td [] [text "intelligencia"], Table.td [] [text <| String.fromInt state.user.skills.intelligencia] ] |> Table.tr []
        , [ Table.td [] [text "diplomácia"], Table.td [] [text <| String.fromInt state.user.skills.diplomacia] ] |> Table.tr []
        , [ Table.td [] [text "hegyesség"], Table.td [] [text <| String.fromInt state.user.skills.hegyesseg] ] |> Table.tr []
        , [ Table.td [] [text "szerencse"], Table.td [] [text <| String.fromInt state.user.skills.szerencse] ] |> Table.tr []
        , [ Table.td [] [text "bájosság"], Table.td [] [text <| String.fromInt state.user.skills.baj] ] |> Table.tr []
        , [ Table.td [] [text "meggyőzőerő"], Table.td [] [text <| String.fromInt state.user.skills.meggyozoero] ] |> Table.tr []
        , [ Table.td [] [text "precízitás"], Table.td [] [text <| String.fromInt state.user.skills.precizitas] ] |> Table.tr []
        , [ Table.td [] [text "nyelvtudás"], Table.td [] [text <| String.fromInt state.user.skills.nyelvtudas] ] |> Table.tr []
        , [ Table.td [] [text "ízlés"], Table.td [] [text <| String.fromInt state.user.skills.izles] ] |> Table.tr []
        , [ Table.td [] [text "vérnyomás"], Table.td [] [text <| String.fromInt state.user.skills.vernyomas] ] |> Table.tr []
        , [ Table.td [] [text "humorérzék"], Table.td [] [text <| String.fromInt state.user.skills.humorerzek] ] |> Table.tr []
        , [ Table.td [] [text "regeneráció"], Table.td [] [text <| String.fromInt state.user.skills.regeneracio] ] |> Table.tr []
        , [ Table.td [] [text "művészlélek"], Table.td [] [text <| String.fromInt state.user.skills.muveszlelek] ] |> Table.tr []
        , [ Table.td [] [text "tisztaságmánia"], Table.td [] [text <| String.fromInt state.user.skills.tisztasagmania] ] |> Table.tr []
        , [ Table.td [] [text "edzettség"], Table.td [] [text <| String.fromInt state.user.skills.edzettseg] ] |> Table.tr []
        ]
        |> Table.tbody []
      }
    ] |> Grid.col []
  ] |> Grid.row []
