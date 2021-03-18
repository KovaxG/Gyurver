module Cokkolo2021.Views.Egg exposing (..)

import Html exposing (Html, text, h2, div, br)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Cokkolo2021.Common exposing (..)
import Cokkolo2021.Views.Contestants as Contestants exposing (Contestant)

type alias ViewState =
  { contestant : Contestant
  , contestantsState : Contestants.ViewState
  }

init : Contestants.ViewState -> Contestant -> ViewState
init cState contestant =
  { contestant = contestant
  , contestantsState = cState
  }

type Message
  = SwitchToContestantsView

view : ViewState -> Html Message
view state =
  [ [ Button.button
      [ Button.outlinePrimary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToContestantsView
      ] [text "Vissza"]
    , h2 [] [text <| state.contestant.eggname]
    , text <| "Gazda: " ++ state.contestant.username
    , br [] []
    , displayImage state.contestant.image 250 250
    ] |> Grid.col []
  , [ Table.table
        { options = [ Table.striped ]
        , thead =
            Table.simpleThead
                [ Table.th [] [text "Elnevezes"]
                , Table.th [] [text "Pontok"]
                ]
        , tbody =
          [ Table.tr [] [Table.td [] [text "keménység"], Table.td [] [text <| String.fromInt state.contestant.skills.kemenyseg]]
          , Table.tr [] [Table.td [] [text "erősség"], Table.td [] [text <| String.fromInt state.contestant.skills.erosseg]]
          , Table.tr [] [Table.td [] [text "settenkedés"], Table.td [] [text <| String.fromInt state.contestant.skills.settenkedes]]
          , Table.tr [] [Table.td [] [text "szivarozás"], Table.td [] [text <| String.fromInt state.contestant.skills.szivarozas]]
          , Table.tr [] [Table.td [] [text "furfangosság"], Table.td [] [text <| String.fromInt state.contestant.skills.furfangossag]]
          , Table.tr [] [Table.td [] [text "tűzokádás"], Table.td [] [text <| String.fromInt state.contestant.skills.tuzokadas]]
          , Table.tr [] [Table.td [] [text "zsírosság"], Table.td [] [text <| String.fromInt state.contestant.skills.zsirossag]]
          , Table.tr [] [Table.td [] [text "intelligencia"], Table.td [] [text <| String.fromInt state.contestant.skills.intelligencia]]
          , Table.tr [] [Table.td [] [text "diplomácia"], Table.td [] [text <| String.fromInt state.contestant.skills.diplomacia]]
          , Table.tr [] [Table.td [] [text "hegyesség"], Table.td [] [text <| String.fromInt state.contestant.skills.hegyesseg]]
          , Table.tr [] [Table.td [] [text "szerencse"], Table.td [] [text <| String.fromInt state.contestant.skills.szerencse]]
          , Table.tr [] [Table.td [] [text "bájosság"], Table.td [] [text <| String.fromInt state.contestant.skills.baj]]
          , Table.tr [] [Table.td [] [text "meggyőzőerő"], Table.td [] [text <| String.fromInt state.contestant.skills.meggyozoero]]
          , Table.tr [] [Table.td [] [text "precízitás"], Table.td [] [text <| String.fromInt state.contestant.skills.precizitas]]
          , Table.tr [] [Table.td [] [text "nyelvtudás"], Table.td [] [text <| String.fromInt state.contestant.skills.nyelvtudas]]
          , Table.tr [] [Table.td [] [text "ízlés"], Table.td [] [text <| String.fromInt state.contestant.skills.izles]]
          , Table.tr [] [Table.td [] [text "vérnyomás"], Table.td [] [text <| String.fromInt state.contestant.skills.vernyomas]]
          , Table.tr [] [Table.td [] [text "humorérzék"], Table.td [] [text <| String.fromInt state.contestant.skills.humorerzek]]
          , Table.tr [] [Table.td [] [text "regeneráció"], Table.td [] [text <| String.fromInt state.contestant.skills.regeneracio]]
          , Table.tr [] [Table.td [] [text "művészlélek"], Table.td [] [text <| String.fromInt state.contestant.skills.muveszlelek]]
          , Table.tr [] [Table.td [] [text "tisztaságmánia"], Table.td [] [text <| String.fromInt state.contestant.skills.tisztasagmania]]
          , Table.tr [] [Table.td [] [text "edzettség"], Table.td [] [text <| String.fromInt state.contestant.skills.edzettseg]]
          ] |> Table.tbody []
        }
    ] |> Grid.col []
  ] |> Grid.row []
