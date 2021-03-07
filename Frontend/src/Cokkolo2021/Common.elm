module Cokkolo2021.Common exposing (..)

import Html exposing (Html, button, div, text, h1, h2, h3, p, ol, li, br, a, img)
import Html.Attributes exposing (src, alt, style)
import Json.Decode as Decode exposing (Decoder)

type ViewStatus = Normal | Waiting | Problem String

type alias User =
  { username : String
  , password : String
  , eggName : String
  , perfume : Int
  , image : String
  }

userDecoder : Decoder User
userDecoder =
  Decode.map5
    User
    (Decode.field "username" Decode.string)
    (Decode.field "password" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "perfume" Decode.int)
    (Decode.field "image" Decode.string)

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