module Cokkolo2021.Views.Egg exposing (..)

import Html exposing (Html, text, h2, div, br)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Cokkolo2021.Common exposing (..)
import Cokkolo2021.Types.Fight as Fight
import Cokkolo2021.Views.Contestants as Contestants exposing (Contestant)
import Settings

type alias ViewState =
  { user : User
  , contestant : Contestant
  , contestantsState : Contestants.ViewState
  }

init : Contestants.ViewState -> User -> Contestant -> ViewState
init cState user contestant =
  { user = user
  , contestant = contestant
  , contestantsState = cState
  }

type Message
  = SwitchToContestantsView
  | FightRequest User Contestant
  | FightRequestFailure String
  | FightRequestSuccess (List Fight.FightLog)

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
    , displayEgg state.contestant.base.image
    , br [] []
    , text <| "Ennyiszer öntözték meg: " ++ String.fromInt state.contestant.ontoztek
    , br [] []
    , text <| "Ennyiszer öntözött másokat: " ++ String.fromInt state.contestant.ontozott
    , br [] []
    , br [] []
    , Button.button
      [ Button.outlineDanger
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick <| FightRequest state.user state.contestant
      ] [text "Összecökkentés"]
    ] |> Grid.col []
  , [ Table.table
        { options = [ Table.striped ]
        , thead =
            Table.simpleThead
                [ Table.th [] [text "Elnevezés"]
                , Table.th [] [text "Szint"]
                ]
        , tbody =
          [ ("keménység", state.contestant.skills.kemenyseg)
          , ("erősség", state.contestant.skills.erosseg)
          , ("settenkedés", state.contestant.skills.settenkedes)
          , ("szivarozás", state.contestant.skills.szivarozas)
          , ("furfangosság", state.contestant.skills.furfangossag)
          , ("tűzokádás", state.contestant.skills.tuzokadas)
          , ("zsírosság", state.contestant.skills.zsirossag)
          , ("intelligencia", state.contestant.skills.intelligencia)
          , ("diplomácia", state.contestant.skills.diplomacia)
          , ("hegyesség", state.contestant.skills.hegyesseg)
          , ("szerencse", state.contestant.skills.szerencse)
          , ("bájosság", state.contestant.skills.baj)
          , ("meggyőzőerő", state.contestant.skills.meggyozoero)
          , ("precízitás", state.contestant.skills.precizitas)
          , ("nyelvtudás", state.contestant.skills.nyelvtudas)
          , ("ízlés", state.contestant.skills.izles)
          , ("vérnyomás", state.contestant.skills.vernyomas)
          , ("humorérzék", state.contestant.skills.humorerzek)
          , ("regeneráció", state.contestant.skills.regeneracio)
          , ("művészlélek", state.contestant.skills.muveszlelek)
          , ("tisztaságmánia", state.contestant.skills.tisztasagmania)
          , ("edzettség", state.contestant.skills.edzettseg)
          ] |> List.filter (\(_, v) -> v > 0) |> List.map tableRow |> Table.tbody []
        }
    ] |> Grid.col []
  ] |> Grid.row []

tableRow : (String, Int) -> Table.Row Message
tableRow (field, value) = Table.tr [] [Table.td [] [text field], Table.td [] [text <| String.fromInt value ++ "/10"]]