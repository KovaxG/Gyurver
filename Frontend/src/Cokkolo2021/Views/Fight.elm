module Cokkolo2021.Views.Fight exposing (..)

import Html exposing (Html, text, h2, div, br)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Json.Decode as Decode exposing (Decoder)

import Cokkolo2021.Views.Contestants as Contestants exposing (Contestant)
import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { playerEgg : User
  , enemyEgg : Contestant
  , contestantsState : Contestants.ViewState
  , fightLog : List FightLog
  }

init : Contestants.ViewState -> User -> Contestant -> List FightLog -> ViewState
init cvs user contestant fightLog =
  { playerEgg = user
  , enemyEgg = contestant
  , contestantsState = cvs
  , fightLog = fightLog
  }

type Message = NoMsg

view : ViewState -> Html Message
view state = text <| Debug.toString state

type alias Nev = String
type alias HP = Int
type alias DMG = Int
type alias Effect = String

type FightLog
  = StartFight Nev HP Nev HP
  | Win Nev
  | Damage Nev DMG HP (List Effect) Nev DMG HP (List Effect)
  | Effect Effect

decoder : Decoder (List FightLog)
decoder = Decode.list fightLogDecoder

fightLogDecoder : Decoder FightLog
fightLogDecoder =
  Decode.field "type" Decode.string
  |> Decode.andThen (\type_ ->
    case type_ of
      "win" ->
        Decode.map
          Win
          (Decode.field "winner" Decode.string)
      "eff" ->
        Decode.map
          Effect
          (Decode.field "effect" Decode.string)
      "start" ->
        Decode.map4
          StartFight
          (Decode.field "nameA" Decode.string)
          (Decode.field "hpA" Decode.int)
          (Decode.field "nameB" Decode.string)
          (Decode.field "hpB" Decode.int)
      "damage" ->
        Decode.map8
          Damage
          (Decode.field "nameA" Decode.string)
          (Decode.field "dmgA" Decode.int)
          (Decode.field "hpA" Decode.int)
          (Decode.field "effsA" <| Decode.list (Decode.string))
          (Decode.field "nameB" Decode.string)
          (Decode.field "dmgB" Decode.int)
          (Decode.field "hpB" Decode.int)
          (Decode.field "effsB" <| Decode.list (Decode.string))
      other -> Decode.fail <| "I don't recognize this type: " ++ other
  )
