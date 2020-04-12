module Main exposing (..)

import Browser
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text, input, br, h1)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, style)
import Http exposing (post, stringBody, expectWhatever, Error)
import Debug

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

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

init : () -> (Model, Cmd Msg)
init _ =
  let
    kezdeti =
      { nev = "asdsa"
      , szin = Piros
      }
  in (kezdeti, Cmd.none)

type Msg
  = NevValtozott String
  | SzinValtozott Szin
  | Adatkuldes
  | MindenOk (Result Error ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NevValtozott nev ->
      let ujModel = { model | nev = nev }
      in (ujModel, Cmd.none)
    SzinValtozott szin ->
      let ujModel = { model | szin = szin }
      in (ujModel, Cmd.none)
    Adatkuldes ->
      let
        parancs =
          post
          { url = "/cokk/add"
          , body = stringBody "text/html" (toHaskellNotation model)
          , expect = expectWhatever MindenOk
          }
      in (model, parancs)
    MindenOk _ -> (model, load "/cokk/list")

view : Model -> Html Msg
view model =
  if String.isEmpty model.nev
  then nevbeiras model
  else tojasKep model

nevbeiras : Model -> Html Msg
nevbeiras model =
  div []
    [ h1 [] [text "Új tojás!"]
    , text "Tojás Neve: "
    , input [placeholder "A tojás neve", value model.nev, onInput NevValtozott] []
    ]

tojasKep : Model -> Html Msg
tojasKep model =
  div []
    [ h1 [] [text "Új tojás!"]
    , text "Tojás Neve: "
    , input [placeholder "A tojás neve", value model.nev, onInput NevValtozott] []
    , br [] []
    , tojaskepek model
    ]


tojaskepek : Model -> Html Msg
tojaskepek model =
  div []
    [ div [ style "position" "absolute"
          , style "top" "150px"
          , style "left" "20px"
          , style "width" "126px"
          , style "height" "180px"
          , style "padding" "20px"
          , style "background" (toString model.szin)
          , style "border-radius" "50% 50% 50% 50% / 60% 60% 40% 40%"
          , style "text-align" "center"
          , style "vertical-align" "middle"
          , style "line-height" "180px"
          , onClick Adatkuldes
          ]
          [ text "Küldés"
          ]
    , szinValaszto
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
  div
    [ style "position" "absolute"
    , style "top" "150px"
    , style "left" "240px"
    ]
    [ minta Piros
    , minta Sarga
    , minta Zold
    , minta Kek
    ]

toHaskellNotation : Model -> String
toHaskellNotation model =
  "Tojas { nev = \"" ++ model.nev ++ "\", hatterSzin = " ++ Debug.toString model.szin ++ ", motivum = Semmi }"