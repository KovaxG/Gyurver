module Cokkolo2021.Views.Fight exposing (..)

import Html exposing (Html, button, div, text, h1, h2, h3, p, ol, li, br, a, img)
import Html.Attributes exposing (src, alt, style)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Progress as Progress

import Cokkolo2021.Types.Fight as Fight
import Cokkolo2021.Views.Contestants as Contestants exposing (Contestant)
import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { playerEgg : User
  , enemyEgg : Contestant
  , contestantsState : Contestants.ViewState
  , fightLog : List Fight.FightLog
  , displayedLog : List Fight.FightLog
  , playerMaxHP : Fight.HP
  , playerHP : Fight.HP
  , enemyMaxHP : Fight.HP
  , enemyHP : Fight.HP
  }

init : Contestants.ViewState -> User -> Contestant -> List Fight.FightLog -> ViewState
init cvs user contestant fightLog =
  { playerEgg = user
  , enemyEgg = contestant
  , contestantsState = cvs
  , fightLog = fightLog
  , displayedLog = []
  , playerMaxHP = 40
  , playerHP = 40
  , enemyMaxHP = 40
  , enemyHP = 40
  } |> cycle

type Message
  = NextMessage
  | SwitchToContestantsView

cycle : ViewState -> ViewState
cycle state =
  let newFightLogs = List.tail state.fightLog |> Maybe.withDefault []
      eventOpt = List.head state.fightLog
      newDisplayedLog = eventOpt |> Maybe.map (\h -> state.displayedLog ++ [h]) |> Maybe.withDefault state.displayedLog
      updateLogs s = { s | fightLog = newFightLogs, displayedLog = newDisplayedLog }
  in Maybe.map (\e -> updateState e state) eventOpt |> Maybe.withDefault state |> updateLogs


updateState : Fight.FightLog -> ViewState -> ViewState
updateState fl s = case fl of
  Fight.StartFight n1 hp1 _ hp2 ->
    let (playerHP, enemyHP) = if n1 == s.playerEgg.eggName then (hp1, hp2) else (hp2, hp1)
    in { s |  playerMaxHP = playerHP, playerHP = playerHP, enemyMaxHP = enemyHP, enemyHP = enemyHP }
  Fight.Damage n1 _ hp1 _ _ _ hp2 _ ->
    let (playerHP, enemyHP) = if n1 == s.playerEgg.eggName then (hp1, hp2) else (hp2, hp1)
    in { s |  playerHP = playerHP, enemyHP = enemyHP }
  _ -> s

hpBar : Fight.HP -> Fight.HP -> Html Message
hpBar hp maxHP =
  let pp = 100 * toFloat hp / toFloat maxHP
      p = if pp <= 0 then 0 else pp
  in Progress.progressMulti [[Progress.value p, Progress.success], [Progress.value (100 - p), Progress.danger]]

view : ViewState -> Html Message
view state =
  [ Button.button
    [ Button.outlineSecondary
    , Button.attrs [ Spacing.m2 ]
    , Button.onClick SwitchToContestantsView
    ]
    [ text "Vissza" ]
  , [ [ h2 [] [text state.playerEgg.eggName]
      , displayImage state.playerEgg.base.image 0 0
      , br [] []
      , hpBar state.playerHP state.playerMaxHP
      , text <| "❤️ " ++ String.fromInt state.playerHP ++ "/" ++ String.fromInt state.playerMaxHP
      ] |> Grid.col [Col.xs3]
    , [ h2 [] [text "A harc"]
      , List.map (\l -> div [] [text <| Fight.toString l]) state.displayedLog |> div []
      , if Nothing == List.head state.fightLog
        then
          text ""
        else
          Button.button
            [ Button.info
            , Button.onClick NextMessage
            ]
            [ text "Tovább" ]
      ] |> Grid.col []
    , [ h2 [] [text state.enemyEgg.eggname]
      , displayImage state.enemyEgg.base.image 0 0
      , br [] []
      , hpBar state.enemyHP state.enemyMaxHP
      , text <| "❤️ " ++ String.fromInt state.enemyHP ++ "/" ++ String.fromInt state.enemyMaxHP
      ] |> Grid.col [Col.xs3]
    ] |> Grid.row []
  ] |> div []

displayImage : String -> Int -> Int -> Html a
displayImage url width height = img
  [ src url
  , alt "Jaj ne! Nem töltödött be a kép! Most mi lesz? 😢 Pls szólj Gyurinak"
  , style "width" <| if width > 0 then (String.fromInt width ++ "px") else "100%"
  ] []
