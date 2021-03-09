module Cokkolo2021.Views.Dashboard exposing (..)

import Html exposing (Html, div, text, h2, br)
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing

import Json.Decode as Decode exposing (Decoder)
import List.Extra as List

import Types.DateTime as DateTime exposing (DateTime)
import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { user : User
  , logs : List Log
  }

decode : Decoder ViewState
decode =
  Decode.map2
    ViewState
    (Decode.field "user" userDecoder)
    (Decode.field "events" <| Decode.list log)

populateTemporary : User -> ViewState
populateTemporary user = { user = user, logs = [] }

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

type Message
  = FetchSuccess ViewState
  | FetchFailure String
  | Logout
  | SwitchToContestantView
  | SwitchToSkillsView

view : ViewState -> Html Message
view state =
  [ [ h2 [] [text <| state.user.eggName]
    , displayImage state.user.image 250 250
    , Button.button
      [ Button.outlinePrimary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToSkillsView
      ] [text "Tulajdonságok"]
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
