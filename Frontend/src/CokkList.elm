module CokkList exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text, h1, h3, p, ol, li, br, a)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, href)
import Http exposing (get, expectJson, Error)
import Json.Decode exposing (Decoder)
import Json.Decode as Decoder

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text

type alias Model =
  { tojasok : List Tojas
  , betoltve : Bool
  }
  
type alias Tojas =
  { nev : String
  , szin : String
  }
  
type Msg
  = Tojasok (Result Error (List Tojas))
  | UjTojasOldal
  
init : (Model, Cmd Msg)
init =
  let
    kezdeti = { tojasok = [], betoltve = False }
    parancs = get
      { url = "/cokk/list"
      , expect = expectJson Tojasok (Decoder.list tojasDecoder)
      }
  in (kezdeti, parancs)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tojasok (Err err) ->
      ( { model | betoltve = True }
      , Cmd.none
      )
    Tojasok (Ok tojasok) ->
      ( { model | tojasok = tojasok, betoltve = True }
      , Cmd.none
      )
    UjTojasOldal -> (model, load "/cokk/add")

view : Model -> Document Msg
view model =
  { title = "Cokk List"
  , body = 
    [ [ CDN.stylesheet
      , [ [ h1 [] [text "2020 Húsvéti játékok"]
          , leiras
          , text "A felíratkozásnak már vége, a nap folyamán itt lesz egy link ahol majd meg lehet nézni az eredményeket. Sok sikert mindenkinek :)"
          , br [] []
          , a [href "/cokk/eredmeny"] [text "Mutasd az Eredményeket!"]
          , h3 [] [text "Versenyzők"]
          , if model.betoltve
            then listaView model.tojasok
            else Spinner.spinner [Spinner.color Text.danger] []
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

listaView : List Tojas -> Html Msg
listaView tojasok =
  if List.isEmpty tojasok
  then div [] [text "Még nincs jelentkező, lehetnél az első :D"]
  else ol [] (List.map toListItem tojasok)

toListItem : Tojas -> Html Msg
toListItem tojas =
  let
    tojasPic =
      div [ style "float" "left"
          , style "top" "-15px"
          , style "width" "15px"
          , style "height" "23px"
          , style "padding" "2px"
          , style "background" tojas.szin
          , style "border-radius" "50% 50% 50% 50% / 60% 60% 40% 40%"
          ] []
  in li [] [tojasPic, text tojas.nev]

tojasDecoder : Decoder Tojas
tojasDecoder =
  Decoder.map2 Tojas
    (Decoder.field "nev" Decoder.string)
    (Decoder.field "szin" Decoder.string)

leiras =
  p []
    [ text "Kellemes ünnepeket! Üdv az első online cökkölési versenyen."
    , br [] []
    , br [] []
    , text "Mi az a cökkölés? Egyesek kocogtatásnak nevezik, de egy játék, ahol két résztvevő egy-egy húsvéti tojást kiválaszt és a két tojást összeütik. Akinek eltörik a tojása, az veszít."
    , br [] []
    , text "Mivel mostanság nem nagyon mehetünk ki, gondoltam hogy az online világba viszem ezt a játékot."
    , br [] []
    , br [] []
    , text "Nos, mostantól egész Április 13ig fel lehet iratkozni egy tojással, és majd Április 14én egy tournament stílusban összecökkentem a tojásokat (digitálisan), amíg egy nyertes marad."
    , br [] []
    , br [] []
    , text "Mi a nyeremény? Hát, legyen egy hivatalosan aláírt bizonyitvány, hogy a 2020 Cökkölési verseny nyertese vagy, és hogy neked volt a legkeményebb tojásod. Ezt linkelheted a LinkedInes profilodra. Ha akarod."
    ]
