module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, style)
import Debug

main =
  Browser.sandbox { init = init, update = update, view = view }

type Szin = Piros | Sarga | Zold | Kek

type alias Model =
  { nev : String
  , szin : Szin
  }

toString : Szin -> String
toString szin = case szin of
  Piros -> "red"
  Sarga -> "yellow"
  Zold -> "green"
  Kek -> "blue"

init : Model
init =
  { nev = ""
  , szin = Piros
  }

type Msg
  = NevValtozott String
  | SzinValtozott Szin


update : Msg -> Model -> Model
update msg model =
  case msg of
    NevValtozott nev -> { model | nev = nev }
    SzinValtozott szin -> { model | szin = szin }

view : Model -> Html Msg
view model =
  div []
    [ text "Tojás Neve: "
    , input [placeholder "A tojás neve", value model.nev, onInput NevValtozott] []
    , div [ style "width" "126px"
          , style "height" "180px"
          , style "padding" "20px"
          , style "background" (toString model.szin)
          , style "border-radius" "50% 50% 50% 50% / 60% 60% 40% 40%"
          ] []
    , szinValaszto
    , br [] []
    , button [] [text "Add"]
    , text <| toHaskellNotation model
    ]

szinValaszto : Html Msg
szinValaszto =
  let
    minta : Szin -> Html Msg
    minta szin =
      div [ style "width" "50px"
          , style "height" "50px"
          , style "background" (toString szin)
          , onClick (SzinValtozott szin)
          ]
          []
  in
  div []
    [ minta Piros
    , minta Sarga
    , minta Zold
    , minta Kek
    ]

toHaskellNotation : Model -> String
toHaskellNotation model =
  "Tojas { nev = \"" ++ model.nev ++ "\", hatterSzin = " ++ Debug.toString model.szin ++ ", motivum = Semmi }"