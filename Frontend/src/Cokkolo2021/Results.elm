module Cokkolo2021.Results exposing (..)

import Html exposing (Html, div, h1, h2, h3, text)
import Html.Attributes exposing (style)
import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Table as Table
import Bootstrap.Grid as Grid

import Cokkolo2021.Common exposing (..)
import Html.Attributes exposing (rowspan)

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
          , bracket
          , h2 [] [text "Selejtező"]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "A" [(pucer, "Névtelen Tojás", 3), (pucer, "Tojika", 2), (pucer, "Repedés", 2)]
          ] |> Grid.col []
        , [ csoport "B" [(pucer, "Már tudja a halál mi volt a nevem", 4), (pucer, "kismi19 Tojása", 2), (pucer, "Hezirisz Tojása", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "C" [(legke, "Legkemenyebb", 4), (pucer, "Tokmag Tojása", 2), (pucer, "bloazs Tojása", 0)]
          ] |> Grid.col []
        , [ csoport "D" [(pucer, "Tűnő árnyék Tojása", 4), (sanya, "Agi Tojása", 2), (pucer, "andnor Tojása", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "E" [(pucer, "RETEK Tojása", 4), (carto, "Cartoony", 3), (bimrk, "Stay Frosty", 1)]
          ] |> Grid.col []
        , [ csoport "F" [(maszk, "Tojgli", 4), (pucer, "Összekoccanunk Tojása", 2), (pucer, "VaranTavers Tojása", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      , [ [ csoport "G" [(krisz, "Kriszti Tojása", 4), (pucer, "Terebesi Tojása", 2), (pucer, "Ceci Tojása", 0)]
          ] |> Grid.col []
        , [ csoport "H" [(kehim, "Karola Tojása", 6), (kalap, "Mushou", 4), (sokek, "Cökkmesztőr2077", 2), (pucer, "Tms Tojása", 0)]
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

bracket : Html Msg
bracket =
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
      [ Table.tr []
        [ Table.td [] [displayImage pucer 50 0, text "Névtelen Tojás"]
        , Table.td [Table.cellAttr (rowspan 2)] [displayImage pucer 50 0, text "Névtelen Tojás"]
        , Table.td [Table.cellAttr (rowspan 4)] [displayImage pucer 50 0, text "Névtelen Tojás"]
        , Table.td [Table.cellAttr (rowspan 8), Table.cellAttr (style "valign" "middle")] [displayImage pucer 50 0, text "Névtelen Tojás"]
        ]
      , Table.tr [] [ Table.td [] [displayImage pucer 50 0, text "Már tudja a halál mi volt a nevem"] ]
      , Table.tr []
        [ Table.td [] [displayImage legke 50 0, text "Legkemenyebb"]
        , Table.td [Table.cellAttr (rowspan 2)] [displayImage legke 50 0, text "Legkemenyebb"]
        ]
      , Table.tr [] [ Table.td [] [displayImage pucer 50 0, text "Tűnő árnyék Tojása"] ]
      , Table.tr []
        [ Table.td [] [displayImage pucer 50 0, text "RETEK Tojása"]
        , Table.td [Table.cellAttr (rowspan 2)] [displayImage pucer 50 0, text "RETEK Tojása"]
        , Table.td [Table.cellAttr (rowspan 4)] [displayImage pucer 50 0, text "RETEK Tojása"]
        ]
      , Table.tr [] [ Table.td [] [displayImage maszk 50 0, text "Tojgli"] ]
      , Table.tr []
        [ Table.td [] [displayImage krisz 50 0, text "Kriszti Tojása"]
        , Table.td [Table.cellAttr (rowspan 2)] [displayImage kehim 50 0, text "Karola Tojása"]
        ]
      , Table.tr [] [ Table.td [] [displayImage kehim 50 0, text "Karola Tojása"] ]
      ] |> Table.tbody []
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