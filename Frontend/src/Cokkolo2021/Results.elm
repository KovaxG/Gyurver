module Cokkolo2021.Results exposing (..)

import Html exposing (Html, div, h1, h2, h3, text)
import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Table as Table
import Bootstrap.Grid as Grid

import Cokkolo2021.Common exposing (..)

type alias Model = {}
type Msg = NoMsg

init : (Model, Cmd Msg)
init = ({}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ m = (m, Cmd.none)

view : Model -> Document Msg
view _ =
  { title = "Cokkolo 2021 Results"
  , body =
    [ [ CDN.stylesheet
      , [ [ h1 [] [text "Cökkölés 2021 Eredmények"]
          , h2 [] [text "Selejtező"]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "A" [(pucer, "Tojika", 0), (pucer, "Repedés", 0), (pucer, "A Névtelen Tojás", 0)]
          ] |> Grid.col []
        , [ csoport "B" [(pucer, "kismi19 Tojása", 0), (pucer, "Már tudja a halál mi volt a nevem", 0), (pucer, "Hezirisz Tojása", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "C" [(pucer, "Tokmag Tojása", 0), (legke, "Legkemenyebb", 0), (pucer, "bloazs Tojása", 0)]
          ] |> Grid.col []
        , [ csoport "D" [(pucer, "andnor Tojása", 0), (sanya, "Agi Tojása", 0), (pucer, "Tűnő árnyék Tojása", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "E" [(pucer, "RETEK Tojása", 0), (carto, "Cartoony", 0), (bimrk, "Stay Frosty", 0)]
          ] |> Grid.col []
        , [ csoport "F" [(maszk, "Tojgli", 0), (pucer, "VaranTavers Tojása", 0), (pucer, "Összekoccanunk Tojása", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "G" [(krisz, "Kriszti Tojása", 0), (pucer, "Terebesi Tojása", 0), (pucer, "Ceci Tojása", 0)]
          ] |> Grid.col []
        , [ csoport "H" [(sokek, "Cökkmesztőr2077", 0), (pucer, "Tms Tojása", 0), (kehim, "Karola Tojása", 0), (kalap, "Mushou", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

csoport : String -> List (String, String, Int) -> Html Msg
csoport betu tojasok =
  [ h3 [] [text <| betu ++ " Csoport"]
  , Table.table
      { options = [ Table.striped, Table.bordered ]
      , thead =
          Table.simpleThead
              [ Table.th [] [text "Tojás"]
              , Table.th [] [text "Pontszám"]
              ]
      , tbody =
        tojasok
        |> List.map (\(pic, name, score) ->
          Table.tr [] [ Table.td [] [displayImage pic 50 0, text name], Table.td [] [text <| String.fromInt score]]
        )
        |> Table.tbody []
      }
  ] |> div []

pucer = "https://i.postimg.cc/050KpBJb/pp.png"
legke = "https://i.postimg.cc/brC0yCxY/pinkbriton.png"
carto = "https://i.postimg.cc/SRkXBWHS/rozsalevel.png"
bimrk = "https://i.postimg.cc/1XrFXWDV/bismark.png"
maszk = "https://i.postimg.cc/BvP1HZx0/szabalyosmaszkviseles.png"
krisz = "https://i.postimg.cc/28HqtLGY/pirosminta.png"
sokek = "https://i.postimg.cc/prbQph74/navyblue.png"
kehim = "https://i.postimg.cc/9fDybYHs/kekminta.png"
kalap = "https://i.postimg.cc/9QFdRj8m/kalap.png"
sanya = "https://i.postimg.cc/wjfhDbZh/hosszulevel.png"