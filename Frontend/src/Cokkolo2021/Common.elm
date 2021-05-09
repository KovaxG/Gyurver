module Cokkolo2021.Common exposing (..)

import Html exposing (Html, button, div, text, h1, h2, h3, p, ol, li, br, a, img)
import Html.Attributes exposing (src, alt, style)
import Json.Decode as Decode exposing (Decoder)
import Types.Decode as Decode
import Util
import Settings

type ViewStatus = Normal | Waiting | Problem String

type alias User =
  { username : String
  , password : String
  , eggName : String
  , perfume : Int
  , base : Item
  , skills : Skills
  , items : List Int
  }

type alias Item =
  { index : Int
  , name : String
  , image : String
  , cost : Int
  }

itemDecoder : Decoder Item
itemDecoder =
  Decode.map4
    Item
    (Decode.field "index" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "image" Decode.string)
    (Decode.field "cost" Decode.int)

itemsDecoder : Decoder (List Item)
itemsDecoder = Decode.list itemDecoder

userDecoder : Decoder User
userDecoder =
  Decode.map7
    User
    (Decode.field "username" Decode.string)
    (Decode.field "password" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "perfume" Decode.int)
    (Decode.field "base" itemDecoder)
    (Decode.field "skills" skillsDecoder)
    (Decode.field "items" (Decode.list Decode.int))

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

initSkills : Skills
initSkills =
  { kemenyseg = 0
  , erosseg = 0
  , settenkedes = 0
  , szivarozas = 0
  , furfangossag = 0
  , tuzokadas = 0
  , zsirossag = 0
  , intelligencia = 0
  , diplomacia = 0
  , hegyesseg = 0
  , szerencse = 0
  , baj = 0
  , meggyozoero = 0
  , precizitas = 0
  , nyelvtudas = 0
  , izles = 0
  , vernyomas = 0
  , humorerzek = 0
  , regeneracio = 0
  , muveszlelek = 0
  , tisztasagmania = 0
  , edzettseg = 0
  }

modifySkill : String -> (Int -> Int) -> Skills -> Maybe Skills
modifySkill skill f skills = case skill of
  "kemenyseg" -> Just { skills | kemenyseg = f skills.kemenyseg }
  "erosseg" -> Just { skills | erosseg = f skills.erosseg }
  "settenkedes" -> Just { skills | settenkedes = f skills.settenkedes }
  "szivarozas" -> Just { skills | szivarozas = f skills.szivarozas }
  "furfangossag" -> Just { skills | furfangossag = f skills.furfangossag }
  "tuzokadas" -> Just { skills | tuzokadas = f skills.tuzokadas }
  "zsirossag" -> Just { skills | zsirossag = f skills.zsirossag }
  "intelligencia" -> Just { skills | intelligencia = f skills.intelligencia }
  "diplomacia" -> Just { skills | diplomacia = f skills.diplomacia }
  "hegyesseg" -> Just { skills | hegyesseg = f skills.hegyesseg }
  "szerencse" -> Just { skills | szerencse = f skills.szerencse }
  "baj" -> Just { skills | baj = f skills.baj }
  "meggyozoero" -> Just { skills | meggyozoero = f skills.meggyozoero }
  "precizitas" -> Just { skills | precizitas = f skills.precizitas }
  "nyelvtudas" -> Just { skills | nyelvtudas = f skills.nyelvtudas }
  "izles" -> Just { skills | izles = f skills.izles }
  "vernyomas" -> Just { skills | vernyomas = f skills.vernyomas }
  "humorerzek" -> Just { skills | humorerzek = f skills.humorerzek }
  "regeneracio" -> Just { skills | regeneracio = f skills.regeneracio }
  "muveszlelek" -> Just { skills | muveszlelek = f skills.muveszlelek }
  "tisztasagmania" -> Just { skills | tisztasagmania = f skills.tisztasagmania }
  "edzettseg" -> Just { skills | edzettseg = f skills.edzettseg }
  _ -> Nothing

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
displayImage url width height = img
  [ src url
  , alt "Jaj ne! Nem töltödött be a kép! Most mi lesz? 😢 Pls szólj Gyurinak"
  , if height > 0 then style "height" (String.fromInt height ++ "px") else style "" ""
  , if width > 0 then style "width" (String.fromInt width ++ "px") else style "" ""
  ] []

-- TODO use this after the event is over
displayEggLocal : String -> Html a
displayEggLocal name = displayImage (Settings.path ++ "/res/" ++ name) 250 333

displayEgg : String -> Html a
displayEgg name = displayImage name 250 333

encryptPass : String -> String
encryptPass = String.fromInt << List.sum << List.map Char.toCode << String.toList
