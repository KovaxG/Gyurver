module CokkList exposing (..)

import Browser
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text, h1, h3, p, ol, li, br)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Http exposing (get, expectJson, Error)
import Json.Decode exposing (Decoder)
import Json.Decode as Decoder

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

type alias Tojas =
  { nev : String
  , szin : String
  }

type alias Model =
  { tojasok : List Tojas
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    kezdeti = { tojasok = [] }
    parancs = get
      { url = "/cokk/list"
      , expect = expectJson Tojasok (Decoder.list tojasDecoder)
      }
  in (kezdeti, parancs)

type Msg
  = Tojasok (Result Error (List Tojas))
  | UjTojasOldal

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tojasok (Err err) -> (model, Cmd.none)
    Tojasok (Ok tojasok) ->
      let ujModel = { model | tojasok = tojasok }
      in (ujModel, Cmd.none)
    UjTojasOldal -> (model, load "/cokk/add")

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "2020 Húsvéti játékok"]
    , leiras
    , button [onClick UjTojasOldal] [text "Én is részt akarok venni!"]
    , h3 [] [text "Versenyzők"]
    , if List.isEmpty model.tojasok
      then div [] [text "Még nincs jelentkező, lehetnél az első :D"]
      else ol [] (List.map toListItem model.tojasok)
    ]

toListItem : Tojas -> Html Msg
toListItem tojas =
  let
    tojasPic =
      div [ style "float" "left"
          , style "top" "-15px"
          , style "width" "6px"
          , style "height" "9px"
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
    , text "Nos, mostantól egész Április 13ig fel lehet íratkozni egy tojással, és majd Április 14én egy tournament stílusban összecökkentem a tolyásokat (digitálisan), amíg egy nyertes marad."
    , br [] []
    , br [] []
    , text "Mi a nyeremény? Hát, legyen egy hivatalosan aláírt bizonyítvány, hogy a 2020 Cökkölési verseny nyertese vagy, és hogy neked volt a legkeményebb tojásod. Ezt linkelheted a LinkedInes profilodra. Ha akarod."
    ]