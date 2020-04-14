module Eredmenyek exposing (..)

import Browser
import Browser.Navigation exposing (load)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http exposing (get, expectJson, Error)
import Json.Decode exposing (Decoder)
import Json.Decode as Decoder

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Table as Table

main = Browser.sandbox
  { init = init
  , update = update
  , view = view
  }

type alias Model = ()

init : Model
init = ()

type Msg = None

update : Msg -> Model -> Model
update msg model = model

view : Model -> Html Msg
view model =
  Grid.container []
      [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
      , Grid.row []
        [
          Grid.col []
          [ h1 [] [text "2020 Cökkölés Eredmények"]
          , text "A verseny szabályait elmagyarázom majd egy youtube videóban, és a pontszámokat egy másik videóban, ahol va fél órát dobigálok egy dobokockával. Majd teszek linkeket ha sikerűl befejezni őket :)"
          , h2 [] [text "Csoportok"]
          ]
        ]
      , Grid.row []
          [ Grid.col []
            [ h5 [] [text "A Csoport"]
            , csoportTable [(div [] [tojasPic "red", text "heni"], 2), (div [] [tojasPic "red", text "Hímes a tojás, de erős"], 4), (div [] [tojasPic "red", text "Tojásvadász"], 1)]
            ]
          , Grid.col []
            [ h5 [] [text "B Csoport"]
            , csoportTable [(div [] [tojasPic "#F6DADA", text "Lusta vagyok szinezni"], 5), (div [] [tojasPic "#F6DADA", text "DoktorStrong"], 3), (div [] [tojasPic "green", text "Legkemenyebb"], 2)]
            ]
          ]
      , Grid.row []
        [ Grid.col []
          [ h5 [] [text "C Csoport"]
            , csoportTable [(div [] [tojasPic "red", text "Perszeusz"], 3), (div [] [tojasPic "black", text "Csirke"], 2), (div [] [tojasPic "red", text "Törékeny de erős!"], 4)]
          ]
        , Grid.col []
            [ h5 [] [text "D Csoport"]
            , csoportTable [(div [] [tojasPic "yellow", text "Cökkenetes"], 1), (div [] [tojasPic "green", text "Cökkenő számtani nyuladvány"], 3), (div [] [tojasPic "black", text "Faith"], 4)]
            ]
        ]
      , Grid.row []
          [ Grid.col []
            [ h5 [] [text "E Csoport"]
            , csoportTable [(div [] [tojasPic "#F6DADA", text "Purdé"], 3), (div [] [tojasPic "purple", text "Füles"], 5), (div [] [tojasPic "orange", text "Zeltman Kingsford Császár"], 2)]
            ]
          , Grid.col []
            [ h5 [] [text "F Csoport"]
            , csoportTable [(div [] [tojasPic "#F6DADA", text "Mona Lisa"], 2), (div [] [tojasPic "purple", text "CFR"], 2), (div [] [tojasPic "green", text "Kicsi Zöld"], 3)]
            ]
          ]
       , Grid.row []
          [ Grid.col []
            [ h5 [] [text "G Csoport"]
            , csoportTable [(div [] [tojasPic "black", text "Kőtojás"], 1), (div [] [tojasPic "orange", text "Sanya"], 2)]
            ]
          , Grid.col []
            [ h5 [] [text "H Csoport"]
            , csoportTable [(div [] [tojasPic "#F6DADA", text "Keményke"], 0), (div [] [tojasPic "red", text "Nyuszi"], 2)]
            ]
          ]
      , Grid.row []
          [ Grid.col []
            [ h2 [] [text "Verseny Ágak"]
            , brackets
            , text "Idén a legkeményebb tojás biza a \"Törékeny, de erős!\" lett. Kiderült, hogy valójában erős :)"
            ]
          ]
      ]

csoportTable : List (Html Msg, Int) -> Html Msg
csoportTable rows =
  Table.table
    { options = [ Table.striped, Table.bordered ]
    , thead =
        Table.simpleThead
          [ Table.th [] [text "Tojás"]
          , Table.th [] [text "Pontszám"]
          ]
    , tbody =
        Table.tbody []
          (rows |> List.map (\(k, v) -> Table.tr [] [Table.td [] [ k ], Table.td [] [text <| String.fromInt v]]))

    }

tojasPic : String -> Html Msg
tojasPic color =
  div [ style "float" "left"
      , style "top" "-15px"
      , style "width" "15px"
      , style "height" "23px"
      , style "padding" "2px"
      , style "background" color
      , style "border-radius" "50% 50% 50% 50% / 60% 60% 40% 40%"
      ] []

brackets : Html Msg
brackets =
  Table.table
      { options = [ Table.bordered ]
      , thead =
          Table.simpleThead
            [ Table.th [] [text "Negyeddöntő"]
            , Table.th [] [text "Elődöntő"]
            , Table.th [] [text "Döntő"]
            , Table.th [] [text "Nyertes"]
            ]
      , tbody =
          Table.tbody []
            [ Table.tr [] [Table.td [] himes, Table.td [ Table.cellAttr (rowspan 2)] lusta, Table.td [Table.cellAttr (rowspan 4)] torek, Table.td [Table.cellAttr (rowspan 8)] torek]
            , Table.tr [] [Table.td [] lusta ]
            , Table.tr [] [Table.td [] torek, Table.td [Table.cellAttr (rowspan 2)] torek ]
            , Table.tr [] [Table.td [] faith ]
            , Table.tr [] [Table.td [] fules, Table.td [Table.cellAttr (rowspan 2)] fules, Table.td [Table.cellAttr (rowspan 4)] sanya ]
            , Table.tr [] [Table.td [] kiszo ]
            , Table.tr [] [Table.td [] sanya, Table.td [Table.cellAttr (rowspan 2)] sanya ]
            , Table.tr [] [Table.td [] nyusz ]
            ]

      }

himes = [tojasPic "red", text "Hímes a tojás, de erős"]
lusta = [tojasPic "#F6DADA", text "Lusta vagyok szinezni"]
torek = [tojasPic "red", text "Törékeny de erős!"]
faith = [tojasPic "black", text "Faith"]
fules = [tojasPic "purple", text "Füles"]
kiszo = [tojasPic "green", text "Kicsi Zöld"]
sanya = [tojasPic "orange", text "Sanya"]
nyusz = [tojasPic "red", text "Nyuszi"]