module Cokkolo2021.Views.Egg exposing (..)

import Html exposing (Html, text, h2, div, br)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Cokkolo2021.Common exposing (..)
import Cokkolo2021.Views.Contestants as Contestants exposing (Contestant)
import Settings

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
      [ Button.outlineSecondary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToContestantsView
      ] [text "Vissza"]
    , h2 [] [text <| state.contestant.eggname]
    , text <| "Gazda: " ++ state.contestant.username
    , br [] []
    , displayImage (Settings.path ++ "/res/" ++ state.contestant.base.image) 250 250
    , br [] []
    , text <| "Ennyiszer öntözték meg: " ++ String.fromInt state.contestant.ontoztek
    , br [] []
    , text <| "Ennyiszer öntözött másokat: " ++ String.fromInt state.contestant.ontozott
    ] |> Grid.col []
  , [ Table.table
        { options = [ Table.striped ]
        , thead =
            Table.simpleThead
                [ Table.th [] [text "Elnevezés"]
                , Table.th [] [text "Szint"]
                ]
        , tbody =
          [ Table.tr [] [Table.td [] [text "keménység"], Table.td [] [text <| String.fromInt state.contestant.skills.kemenyseg ++ "/10"]]
          , Table.tr [] [Table.td [] [text "erősség"], Table.td [] [text <| String.fromInt state.contestant.skills.erosseg ++ "/10"]]
          , Table.tr [] [Table.td [] [text "settenkedés"], Table.td [] [text <| String.fromInt state.contestant.skills.settenkedes ++ "/10"]]
          , Table.tr [] [Table.td [] [text "szivarozás"], Table.td [] [text <| String.fromInt state.contestant.skills.szivarozas ++ "/10"]]
          , Table.tr [] [Table.td [] [text "furfangosság"], Table.td [] [text <| String.fromInt state.contestant.skills.furfangossag ++ "/10"]]
          , Table.tr [] [Table.td [] [text "tűzokádás"], Table.td [] [text <| String.fromInt state.contestant.skills.tuzokadas ++ "/10"]]
          , Table.tr [] [Table.td [] [text "zsírosság"], Table.td [] [text <| String.fromInt state.contestant.skills.zsirossag ++ "/10"]]
          , Table.tr [] [Table.td [] [text "intelligencia"], Table.td [] [text <| String.fromInt state.contestant.skills.intelligencia ++ "/10"]]
          , Table.tr [] [Table.td [] [text "diplomácia"], Table.td [] [text <| String.fromInt state.contestant.skills.diplomacia ++ "/10"]]
          , Table.tr [] [Table.td [] [text "hegyesség"], Table.td [] [text <| String.fromInt state.contestant.skills.hegyesseg ++ "/10"]]
          , Table.tr [] [Table.td [] [text "szerencse"], Table.td [] [text <| String.fromInt state.contestant.skills.szerencse ++ "/10"]]
          , Table.tr [] [Table.td [] [text "bájosság"], Table.td [] [text <| String.fromInt state.contestant.skills.baj ++ "/10"]]
          , Table.tr [] [Table.td [] [text "meggyőzőerő"], Table.td [] [text <| String.fromInt state.contestant.skills.meggyozoero ++ "/10"]]
          , Table.tr [] [Table.td [] [text "precízitás"], Table.td [] [text <| String.fromInt state.contestant.skills.precizitas ++ "/10"]]
          , Table.tr [] [Table.td [] [text "nyelvtudás"], Table.td [] [text <| String.fromInt state.contestant.skills.nyelvtudas ++ "/10"]]
          , Table.tr [] [Table.td [] [text "ízlés"], Table.td [] [text <| String.fromInt state.contestant.skills.izles ++ "/10"]]
          , Table.tr [] [Table.td [] [text "vérnyomás"], Table.td [] [text <| String.fromInt state.contestant.skills.vernyomas ++ "/10"]]
          , Table.tr [] [Table.td [] [text "humorérzék"], Table.td [] [text <| String.fromInt state.contestant.skills.humorerzek ++ "/10"]]
          , Table.tr [] [Table.td [] [text "regeneráció"], Table.td [] [text <| String.fromInt state.contestant.skills.regeneracio ++ "/10"]]
          , Table.tr [] [Table.td [] [text "művészlélek"], Table.td [] [text <| String.fromInt state.contestant.skills.muveszlelek ++ "/10"]]
          , Table.tr [] [Table.td [] [text "tisztaságmánia"], Table.td [] [text <| String.fromInt state.contestant.skills.tisztasagmania ++ "/10"]]
          , Table.tr [] [Table.td [] [text "edzettség"], Table.td [] [text <| String.fromInt state.contestant.skills.edzettseg ++ "/10"]]
          ] |> Table.tbody []
        }
    ] |> Grid.col []
  ] |> Grid.row []
