module Cokkolo2021.Views.Login exposing (..)

import Html exposing (Html, text, h1, h2, br, div, a)
import Html.Attributes exposing (href, align)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as Encode exposing (Value)

import Types.EventState as EventState
import Cokkolo2021.Common exposing (..)
import Settings

type alias ViewState =
  { username : String
  , password : String
  , state : ViewStatus
  }

init : ViewState
init =
  { username = ""
  , password = ""
  , state = Normal
  }

encodeGeneric : String -> String -> Value
encodeGeneric username password =
  Encode.object
    [ ("user", Encode.string username)
    , ("pass", Encode.string password)
    ]

encode : ViewState -> Value
encode state = encodeGeneric state.username (encryptPass state.password)

type Message
  = UsernameFieldChange String
  | PasswordFieldChange String
  | Login
  | SwitchToRegisterView

view : ViewState -> Html Message
view state =
  [ [ h1 [] [text "2021 Húsvéti játékok"]
    , description
    , h2 [] [text "Belépés"]
    , text "Gazda"
    , Input.text [Input.value state.username, Input.onInput UsernameFieldChange]
    , text "Jelszó"
    , Input.password [Input.value state.password, Input.onInput PasswordFieldChange]
    , case state.state of
        Problem str -> div [] [text str]
        Waiting -> div [] [text "Lássuk csak..."]
        Normal -> text ""
    , Button.button
      [ Button.primary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick Login
      ] [text "Engeddj be!"]
    , if Settings.cokk2021 == EventState.Running
      then
      Button.button
        [ Button.secondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick SwitchToRegisterView
        ] [text "Én is akarok tojást!"]
      else text ""
    , description2
    ] |> Grid.col []
  ] |> Grid.row []

description : Html Message
description =
  [ [ [ div [align "center"] [ displayImage cokk2021logo 0 0] ] |> Grid.col [Col.xs4]
    , [ [ text "Kellemes ünnepeket! Üdv a második online cökkölési versenyen."
        , br [] []
        , br [] []
        , text "Mi az a cökkölés? Egyesek "
        , a [href kocogtatasLink] [text "kocogtatásnak"]
        , text " nevezik, de egy játék, ahol két résztvevő egy-egy húsvéti tojást kiválaszt és a két tojást összeütik. Akinek eltörik a tojása, az veszít. Mivel mostanság nem nagyon mehetünk ki, gondoltam hogy az online világba viszem ezt a játékot (másodjára)."
        , br [] []
        , br [] []
        , Alert.simpleDanger [] [text "Ha a regisztrálással vannak gondok engem el lehet érni facebookon vagy emailen."]
        , Alert.simpleInfo  [] [text "Az öntözésnek vége. Jövő héten valamikor meg fog itt jelenni a végeredményes oldal, köszönöm, hogy résztvettetek, és sok sikert mindenkinek!"]
        ] |> div []
      ] |> Grid.col []
    ] |> Grid.row []
  , br [] []
  , br [] []
  ] |> div []

description2 : Html Message
description2 =
  [ h2 [] [text "Részletek"]
  , text "Nos, mostantól egész Április 11-ig fel lehet íratkozni egy tojással, azt ki lehet fejleszteni meg felszerelni, majd egy tournament stílusban összecökkennek a tojások (digitálisan) amíg egy nyertes marad."
  , br [] []
  , br [] []
  , text "Mi a nyeremény? Hát, legyen egy hivatalosan aláírt bizonyitvány, hogy a 2021 Cökkölési verseny nyertese vagy, és hogy neked volt a legügyesebb tojásod. Ezt linkelheted a LinkedInes profilodra. Ha akarod."
  , br [] []
  , br [] []
  , br [] []
  , br [] []
  ] |> div []

kocogtatasLink = "https://mek.oszk.hu/02100/02115/html/5-683.html"
cokk2021logo = "https://i.postimg.cc/0yVj7HkG/cokk2021logo.png"
