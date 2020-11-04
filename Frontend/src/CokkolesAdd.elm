module CokkolesAdd exposing (..)

import Browser
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text, input, br, h1, p)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, style)
import Http exposing (post, stringBody, expectWhatever, Error)
import Debug

-- DEAD CODE

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

type Szin = Piros | Sarga | Zold | Kek | Narancs | Lila | Fekete | Csore

type alias Model =
  { nev : String
  , szin : Szin
  , uzenet : Html Msg
  }

toString : Szin -> String
toString szin = case szin of
  Piros -> "red"
  Sarga -> "yellow"
  Zold -> "green"
  Kek -> "blue"
  Narancs -> "orange"
  Lila -> "purple"
  Fekete -> "black"
  Csore -> "#F6DADA"

init : () -> (Model, Cmd Msg)
init _ =
  let
    kezdeti =
      { nev = ""
      , szin = Piros
      , uzenet = text "Színezd ki, majd nevezd meg a tojásod!"
      }
  in (kezdeti, Cmd.none)

type Msg
  = NevValtozott String
  | SzinValtozott Szin
  | Adatkuldes
  | Valasz (Result Error ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NevValtozott nev ->
      let
        ujModel =
          { model
          | nev = nev
          , uzenet = if String.isEmpty nev
                     then text "Színezd ki, majd nevezd meg a tojásod!"
                     else text "Remek, nyomd meg a gombot, és mutatom is a listát :)"
          }
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
          , expect = expectWhatever Valasz
          }
      in (model, parancs)
    Valasz (Ok _) -> (model, load "/cokk")
    Valasz (Err err) ->
      let ujModel =
            { model
            | uzenet =
                div []
                  [ text "Hoppá, valami baj lett, nem tudtam elküldeni az adatokat :("
                  , br [] []
                  , text <| "Szólj gyurinak légyszi, hogy a szervertől ezt kaptam vissza: " ++ Debug.toString err
                  ]
            }
      in (ujModel, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Új tojás!"]
    , tojaskepek model
    , if String.isEmpty model.nev
      then
        div []
          [ p [ style "position" "absolute"
              , style "top" "370px"
              ]
              [model.uzenet]
          ]

      else
        div []
        [ button
          [ style "position" "absolute"
          , style "top" "385px"
          , onClick Adatkuldes
          ]
          [text "Ok, szeretnék ezzel a tojással résztvenni!"]
        , p [ style "position" "absolute"
            , style "top" "400px"
            ]
            [model.uzenet]
        ]

    , nevesSor model
    ]

nevesSor : Model -> Html Msg
nevesSor model =
  div
    [ style "position" "absolute"
    , style "top" "350px"
    ]
    [ text "Tojás Neve: "
    , input
      [ placeholder "A tojás neve"
      , value model.nev
      , onInput NevValtozott

      ] []
    ]


tojaskepek : Model -> Html Msg
tojaskepek model =
  div []
    [ div [ style "position" "absolute"
          , style "top" "100px"
          , style "left" "20px"
          , style "width" "126px"
          , style "height" "180px"
          , style "padding" "20px"
          , style "background" (toString model.szin)
          , style "border-radius" "50% 50% 50% 50% / 60% 60% 40% 40%"
          ]
          []
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
          , style "border-radius" "50% 50% 50% 50%"
          , style "border-style" "solid"
          , style "border-width" "1.5px"
          , onClick (SzinValtozott szin)
          ]
          []

    colorCol : Szin -> Szin -> Szin -> Szin -> String -> Html Msg
    colorCol s1 s2 s3 s4 left =
      div
        [ style "position" "absolute"
        , style "top" "100px"
        , style "left" left
        ]
        [ minta s1
        , minta s2
        , minta s3
        , minta s4
        ]

  in
    div []
      [ colorCol Piros Sarga Zold Kek "240px"
      , colorCol Narancs Lila Fekete Csore "292px"
      ]


toHaskellNotation : Model -> String
toHaskellNotation model =
  "Tojas { nev = \"" ++ model.nev ++ "\", hatterSzin = \"" ++ toString model.szin ++ "\" }"