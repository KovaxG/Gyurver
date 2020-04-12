module CokkList exposing (..)

import Browser
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text, input, br, h1, h3, p, ol, li)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, style)
import Http exposing (get, stringBody, expectJson, Error)
import Debug
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
    , p [] [text "bla bla bla"]
    , button [onClick UjTojasOldal] [text "Én is akarok részt venni!"]
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