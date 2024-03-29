module Cokkolo2021.Views.Dashboard exposing (..)

import Html exposing (Html, div, text, h1, h2, h3, br)
import Html.Events exposing (onMouseOver, onMouseLeave, onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Alert as Alert
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Utilities.Spacing as Spacing

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List

import Types.DateTime as DateTime exposing (DateTime)
import Cokkolo2021.Common exposing (..)
import Bootstrap.Popover exposing (onHover)
import Settings

type alias ViewState =
  { user : User
  , logs : List Log
  , showEggnameEdit : Bool
  , eggNameInput : Maybe String
  , eggNameInputError : String
  , suggestionBoxVisibility : Modal.Visibility
  , suggestion : String
  }

decode : Decoder ViewState
decode =
  Decode.map7
    ViewState
    (Decode.field "user" userDecoder)
    (Decode.field "events" <| Decode.list log)
    (Decode.succeed False)
    (Decode.succeed Nothing)
    (Decode.succeed "")
    (Decode.succeed Modal.hidden)
    (Decode.succeed "")

populateTemporary : User -> ViewState
populateTemporary user =
  { user = user
  , logs = []
  , showEggnameEdit = False
  , eggNameInput = Nothing
  , eggNameInputError = ""
  , suggestionBoxVisibility = Modal.hidden
  , suggestion = ""
  }

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
  | ShowSuggestionBox
  | CloseSuggestionBox
  | UpdateSuggestion String
  | SendSuggestion String

view : ViewState -> Html Message
view state =
  [ h1 [] [text "Tojásgarázs"]
  , [ [ Button.button
        [ Button.outlineSecondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick Logout
        ] [text "Kilépés"]
      , case state.eggNameInput of
          Nothing ->
            h2
              [ onMouseOver <| HoweringOverEggName True
              , onMouseLeave <| HoweringOverEggName False
              , onClick <| EditEggName <| Just state.user.eggName
              ]
              [ text <| state.user.eggName ++ if state.showEggnameEdit then " ✍️" else "" ]
          Just en ->
            [ Input.text [Input.value en, Input.onInput <| EditEggName << Just]
            , Button.button [Button.outlineSuccess, Button.disabled <| String.isEmpty <| String.trim en, Button.onClick <| ChangeEggnameRequest en] [text "✔️"]
            , Button.button [Button.outlineDanger, Button.onClick <| EditEggName Nothing ] [text "❌"]
            , text state.eggNameInputError
            ] |> div []
      , displayEgg state.user.base.image
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
      , br [] []
      , br [] []
      , Button.button
        [ Button.outlineInfo
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick ShowSuggestionBox
        ] [text "Javaslati Doboz"]
      , Modal.config CloseSuggestionBox
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Javaslati Doboz" ]
        |> Modal.body []
          [ div []
            [ text "Mi a javaslatod?"
            , Textarea.textarea
              [ Textarea.rows 5
              , Textarea.value state.suggestion
              , Textarea.onInput UpdateSuggestion
              ]
            ]
          ]
        |> Modal.footer []
          [ Button.button
            [ Button.outlineSuccess
            , Button.attrs [ onClick <| SendSuggestion state.suggestion]
            ]
            [ text "Küldés" ]
          , Button.button
            [ Button.outlineDanger
            , Button.attrs [ onClick CloseSuggestionBox ]
            ]
            [ text "Mégse" ]
          ]
        |> Modal.view state.suggestionBoxVisibility
      , Alert.simpleInfo [] [text "A játéknak vége, többet nem lehet öntözni vagy vásárolni vagy fejleszteni. Sok sikert a versenyen, ami valamikor jövöhőten lesz lefuttatva."]
      , Alert.simpleInfo [] [text "Véget ért a regisztrálás. Egyelőre még lehet fejleszteni a tojást és öntözgetni Szerdáig (Április 16). Ez után minden le lesz fagyasztva és csak várni lehet amig bejelentem a csoportokat. A hét végére majd a meccs eredményeket is meg a bajnokot is meg lehet nézni."]
      , Alert.simpleDanger [] [text "Április 11-én egy hiba csúszott a rendszerbe és emiatt mindenkinek eltüntek a fejlesztései és a díszei. Szerencsére épp annyi nem veszlődött el, mert még megvan hogy ki kit öntözött, ezért mindenki visszakaja az összes kölnit amit a héten összegyüjtött és ezeket újra be lehet fektetni amibe csak akarja."]
      ] |> Grid.col []
    , [  br [] []
      , h2 [] [text <| "Kölni: " ++ String.fromInt state.user.perfume ++ " 💦"]
      , br [] []
      , Button.button
        [ Button.primary
        , Button.onClick SwitchToContestantView
        ] [text "Lássam a résztvevőket!"]
      , br [] []
      , br [] []
      , h3 [] [text "Öntözőnapló"]
      , if List.isEmpty state.logs
        then text "Még semmi aktivitás. Öntözzél meg gazdikat a résztvevőlistában, talán ők is megöntöznek téged :)"
        else displayLogs state.user.username state.logs
      ] |> Grid.col []
    ] |> Grid.row []
  ] |> div []

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
        <| List.reverse
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
  1 -> "Január"
  2 -> "Február"
  3 -> "Március"
  4 -> "Április"
  5 -> "Május"
  6 -> "Június"
  7 -> "Július"
  8 -> "Augusztus"
  9 -> "Szeptember"
  10 -> "November"
  11 -> "Október"
  12 -> "December"
  _ -> "Hiba"
