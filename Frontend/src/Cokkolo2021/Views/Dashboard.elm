module Cokkolo2021.Views.Dashboard exposing (..)

import Html exposing (Html, div, text, h2, br)
import Html.Events exposing (onMouseOver, onMouseLeave, onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List

import Types.DateTime as DateTime exposing (DateTime)
import Cokkolo2021.Common exposing (..)
import Bootstrap.Popover exposing (onHover)

type alias ViewState =
  { user : User
  , logs : List Log
  , showEggnameEdit : Bool
  , eggNameInput : Maybe String
  , eggNameInputError : String
  }

decode : Decoder ViewState
decode =
  Decode.map5
    ViewState
    (Decode.field "user" userDecoder)
    (Decode.field "events" <| Decode.list log)
    (Decode.succeed False)
    (Decode.succeed Nothing)
    (Decode.succeed "")

populateTemporary : User -> ViewState
populateTemporary user = { user = user, logs = [], showEggnameEdit = False, eggNameInput = Nothing, eggNameInputError = "" }

type alias Log =
  { source : String
  , target : String
  , datetime : DateTime
  }

log : Decoder Log
log =
  Decode.map3
    Log
    (Decode.field "source" Decode.string)
    (Decode.field "target" Decode.string)
    (Decode.field "time" DateTime.decode)

encodeEggnameChangeRequest : ViewState -> String -> Value
encodeEggnameChangeRequest state newName = Encode.object
  [ ("username", Encode.string state.user.username)
  , ("password", Encode.string state.user.password)
  , ("newEggname", Encode.string newName)
  ]

type Message
  = FetchSuccess ViewState
  | FetchFailure String
  | Logout
  | SwitchToContestantView
  | SwitchToSkillsView
  | SwitchToStoreView
  | HoweringOverEggName Bool
  | EditEggName (Maybe String)
  | ChangeEggnameRequest String
  | ChangeEggnameSuccess String
  | ChangeEggnameFailure String

view : ViewState -> Html Message
view state =
  [ [ case state.eggNameInput of
        Nothing ->
          h2 [ onMouseOver <| HoweringOverEggName True
             , onMouseLeave <| HoweringOverEggName False
             , onClick <| EditEggName <| Just state.user.eggName
             ]
             [ text <| state.user.eggName ++ if state.showEggnameEdit then " ✍️" else "" ]
        Just en ->
          div
            []
            [ Input.text [Input.value en, Input.onInput <| EditEggName << Just]
            , Button.button [Button.outlineSuccess, Button.onClick <| ChangeEggnameRequest en] [text "✔️"]
            , Button.button [Button.outlineDanger, Button.onClick <| EditEggName Nothing ] [text "❌"]
            , text state.eggNameInputError
            ]
    , displayImage state.user.image 250 250
    , br [] []
    , Button.button
      [ Button.outlinePrimary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToSkillsView
      ] [text "Tulajdonságok"]
    , Button.button
      [ Button.outlinePrimary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToStoreView
      ] [text "Üzlet"]
    ] |> Grid.col []
  , [ Button.button
      [ Button.primary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick Logout
      ] [text "Logout"]
      , br [] []
      , text <| "Kölni: " ++ String.fromInt state.user.perfume ++ " 💦"
      , br [] []
      , Button.button
      [ Button.outlineSecondary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToContestantView
      ] [text "Résztvevők"]
      , br [] []
      , displayLogs state.user.username state.logs
      ] |> Grid.col []
  ] |> Grid.row []

displayLogs : String -> List Log -> Html Message
displayLogs username logs =
  div []
  <| List.reverse
  <| List.map (\(byYearEx, byYears) ->
    div []
    <| (++) [text <| String.fromInt byYearEx.datetime.year, br [] []]
    <| List.reverse
    <| List.map (\(byMonthEx, byMonths) ->
      div []
      <| List.reverse
      <| List.map (\(sortedByDayEx, sortedByDays) ->
        div []
        <| (++) [text <| showMonthAndDay sortedByDayEx.datetime]
        <| List.map (\l -> div [] [text <| logToText username l])
        <| List.sortWith (compareOn <| \a -> toFloat (a.datetime.hours * 10000 + a.datetime.minutes * 100) + a.datetime.seconds)
        <| sortedByDayEx :: sortedByDays
      )
      <| List.groupWhile (groupOn <| \a -> a.datetime.day)
      <| List.sortWith (compareOn <| \a -> a.datetime.day)
      <| byMonthEx :: byMonths
    )
    <| List.groupWhile (groupOn <| \a -> a.datetime.month)
    <| List.sortWith (compareOn <| \a -> a.datetime.month)
    <| byYearEx :: byYears
  )
  <| List.groupWhile (groupOn <| \a -> a.datetime.year)
  <| List.sortWith (compareOn <| \a -> a.datetime.year)
  <| logs

logToText : String -> Log -> String
logToText you l =
  let msg =
        if l.source == you
        then "Megöntözted " ++ l.target ++ "-t"
        else "Megontözött " ++ l.source ++ " (+💦)"
      showDigits = String.padLeft 2 '0' << String.fromInt
  in showDigits l.datetime.hours ++ ":"  ++ showDigits l.datetime.minutes ++ " " ++ msg

compareOn : (a -> comparable) -> a -> a -> Order
compareOn f a b = compare (f a) (f b)

groupOn : (a -> b) -> a -> a -> Bool
groupOn f a b = f a == f b

showMonthAndDay : DateTime -> String
showMonthAndDay dt = monthToString dt.month ++ " " ++ String.fromInt dt.day

monthToString : Int -> String
monthToString m = case m of
  1 -> "Jan"
  2 -> "Feb"
  3 -> "Mar"
  4 -> "Apr"
  5 -> "Maj"
  6 -> "Jun"
  7 -> "Jul"
  8 -> "Aug"
  9 -> "Sep"
  10 -> "Nov"
  11 -> "Okt"
  12 -> "Dec"
  _ -> "HOPSZ"
