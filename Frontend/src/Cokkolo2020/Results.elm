module Cokkolo2020.Results exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
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

type Model = NoModel
type Msg = NoMsg

init : (Model, Cmd Msg)
init = (NoModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

type alias RowData =
  { tojasPic : Html Msg
  , score : Int
  }

view : Model -> Document Msg
view model =
  { title = "Eredmenyek"
  , body =
    [ [ CDN.stylesheet
      , [ [ h1 [] [text "2020 Cökkölés Eredmények"]
          , h2 [] [text "Nyertes: Törékeny, de erős!"]
          , p [] [text "Idén a legkeményebb tojás biza a \"Törékeny, de erős!\" lett. Kiderült, hogy valójában erős :)"]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h2 [] [text "Szabályok"]
          , iframe [src "https://www.youtube.com/embed/V01as85s8NY"] []
          ] |> Grid.col []
        , [ h2 [] [text "Meccsek"]
          , p [] [text "Ha ezt látod, akkor még nincs meg a videó, de készűl! (Naaaagyon lassan)"]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h2 [] [text "Verseny Ágak"]
          , brackets
          ] |> Grid.col []
        ] |> Grid.row []
      , [ Grid.col [] [h2 [] [text "Csoportok"]]
        ] |>  Grid.row []
      , [ [ h5 [] [text "A Csoport"]
          , csoportTable [ { tojasPic = div [] [tojasPic "red", text "heni"], score = 2 }
                         , { tojasPic = div [] [tojasPic "red", text "Hímes a tojás, de erős"], score = 4 }
                         , { tojasPic = div [] [tojasPic "red", text "Tojásvadász"], score = 1 }
                         ]
          ] |> Grid.col []
        , [ h5 [] [text "B Csoport"]
          , csoportTable [ { tojasPic = div [] [tojasPic "#F6DADA", text "Lusta vagyok szinezni"], score = 5 }
                         , { tojasPic = div [] [tojasPic "#F6DADA", text "DoktorStrong"], score = 3 }
                         , { tojasPic = div [] [tojasPic "green", text "Legkemenyebb"], score = 2 }
                         ]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h5 [] [text "C Csoport"]
          , csoportTable [ { tojasPic = div [] [tojasPic "red", text "Perszeusz"], score = 3 }
                         , { tojasPic = div [] [tojasPic "black", text "Csirke"], score = 2 }
                         , { tojasPic = div [] [tojasPic "red", text "Törékeny de erős!"], score = 4 }
                         ]
          ] |> Grid.col []
        , [ h5 [] [text "D Csoport"]
          , csoportTable [ { tojasPic = div [] [tojasPic "yellow", text "Cökkenetes"], score = 1 }
                         , { tojasPic = div [] [tojasPic "green", text "Cökkenő számtani nyuladvány"], score = 3 }
                         , { tojasPic = div [] [tojasPic "black", text "Faith"], score = 4 }
                         ]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ h5 [] [text "E Csoport"]
          , csoportTable [ { tojasPic = div [] [tojasPic "#F6DADA", text "Purdé"], score = 3 }
                         , { tojasPic = div [] [tojasPic "purple", text "Füles"], score = 5 }
                         , { tojasPic = div [] [tojasPic "orange", text "Zeltman Kingsford Császár"], score = 2 }
                         ]
          ] |>  Grid.col []
        , [ h5 [] [text "F Csoport"]
          , csoportTable [ { tojasPic = div [] [tojasPic "#F6DADA", text "Mona Lisa"], score = 2 }
                         , { tojasPic = div [] [tojasPic "purple", text "CFR"], score = 2 }
                         , { tojasPic = div [] [tojasPic "green", text "Kicsi Zöld"], score = 3 }
                         ]
          ] |> Grid.col []
        ] |> Grid.row []
        , [ [ h5 [] [text "G Csoport"]
            , csoportTable [ { tojasPic = div [] [tojasPic "black", text "Kőtojás"], score = 1 }
                           , { tojasPic = div [] [tojasPic "orange", text "Sanya"], score = 2 }
                           ]
            ] |> Grid.col []
          , [ h5 [] [text "H Csoport"]
            , csoportTable [ { tojasPic = div [] [tojasPic "#F6DADA", text "Keményke"], score = 0 }
                           , { tojasPic = div [] [tojasPic "red", text "Nyuszi"], score = 2 }
                           ]
            ] |> Grid.col []
          ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

csoportTable : List RowData -> Html Msg
csoportTable rows =
  let max = List.maximum <| List.map .score rows
  in
    Table.table
      { options = [ Table.bordered ]
      , thead =
          Table.simpleThead
            [ Table.th [] [text "Tojás"]
            , Table.th [] [text "Pontszám"]
            ]
      , tbody =
          Table.tbody []
            ( rows
              |> List.map (\rd ->
                Table.tr
                  [if max == Just rd.score then Table.rowSuccess else noAttr]
                  [ Table.td [] [rd.tojasPic]
                  , Table.td [] [text <| String.fromInt rd.score]
                  ]
              )
            )
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

noAttr : Table.RowOption msg
noAttr = Table.rowAttr (style "" "")
