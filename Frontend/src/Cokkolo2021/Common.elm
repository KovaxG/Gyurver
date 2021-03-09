module Cokkolo2021.Common exposing (..)

import Html exposing (Html, button, div, text, h1, h2, h3, p, ol, li, br, a, img)
import Html.Attributes exposing (src, alt, style)
import Json.Decode as Decode exposing (Decoder)
import Types.Decode as Decode
import Util

type ViewStatus = Normal | Waiting | Problem String

type alias User =
  { username : String
  , password : String
  , eggName : String
  , perfume : Int
  , image : String
  , skills : Skills
  }

userDecoder : Decoder User
userDecoder =
  Decode.map6
    User
    (Decode.field "username" Decode.string)
    (Decode.field "password" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "perfume" Decode.int)
    (Decode.field "image" Decode.string)
    (Decode.field "skills" skillsDecoder)

type alias Skills =
  { kemenyseg : Int
  , erosseg : Int
  , settenkedes : Int
  , szivarozas : Int
  , furfangossag : Int
  , tuzokadas : Int
  , zsirossag : Int
  , intelligencia : Int
  , diplomacia : Int
  , hegyesseg : Int
  , szerencse : Int
  , baj : Int
  , meggyozoero : Int
  , precizitas : Int
  , nyelvtudas : Int
  , izles : Int
  , vernyomas : Int
  , humorerzek : Int
  , regeneracio : Int
  , muveszlelek : Int
  , tisztasagmania : Int
  , edzettseg : Int
  }

skillsDecoder : Decoder Skills
skillsDecoder =
  Skills
  |> Util.flip Decode.map (Decode.field "kemenyseg" Decode.int)
  |> Decode.andMap (Decode.field "erosseg" Decode.int)
  |> Decode.andMap (Decode.field "settenkedes" Decode.int)
  |> Decode.andMap (Decode.field "szivarozas" Decode.int)
  |> Decode.andMap (Decode.field "furfangossag" Decode.int)
  |> Decode.andMap (Decode.field "tuzokadas" Decode.int)
  |> Decode.andMap (Decode.field "zsirossag" Decode.int)
  |> Decode.andMap (Decode.field "intelligencia" Decode.int)
  |> Decode.andMap (Decode.field "diplomacia" Decode.int)
  |> Decode.andMap (Decode.field "hegyesseg" Decode.int)
  |> Decode.andMap (Decode.field "szerencse" Decode.int)
  |> Decode.andMap (Decode.field "baj" Decode.int)
  |> Decode.andMap (Decode.field "meggyozoero" Decode.int)
  |> Decode.andMap (Decode.field "precizitas" Decode.int)
  |> Decode.andMap (Decode.field "nyelvtudas" Decode.int)
  |> Decode.andMap (Decode.field "izles" Decode.int)
  |> Decode.andMap (Decode.field "vernyomas" Decode.int)
  |> Decode.andMap (Decode.field "humorerzek" Decode.int)
  |> Decode.andMap (Decode.field "regeneracio" Decode.int)
  |> Decode.andMap (Decode.field "muveszlelek" Decode.int)
  |> Decode.andMap (Decode.field "tisztasagmania" Decode.int)
  |> Decode.andMap (Decode.field "edzettseg" Decode.int)

displayImage : String -> Int -> Int -> Html a
displayImage image width height = img
  [ src <| getImageURL image
  , alt "Jaj ne nem töltödött be a kép! Most mi lesz 😢 Pls szólj Gyurinak"
  , style "height" (String.fromInt height ++ "px")
  , style "width" (String.fromInt width ++ "px")
  ] []

getImageURL : String -> String
getImageURL name = case name of
  "pucer" -> "https://www.pinclipart.com/picdir/middle/68-682374_egg-balancing-by-ofirma85-fnaf-puppet-pixel-art.png"
  _ -> "https://clipground.com/images/omg-emoji-clipart-3.jpg"