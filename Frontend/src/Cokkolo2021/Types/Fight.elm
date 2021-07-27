module Cokkolo2021.Types.Fight exposing (..)

import Json.Decode as Decode exposing (Decoder)

describeEffect : String -> String -> Effect -> String
describeEffect name1 name2 eff = case eff of
  "ZsirossagCheck" -> name1 ++ " extra zsíros, a támadasok lecsusznak róla."
  "BajCheck" -> name1 ++ " bája teljesen elvarázsolta " ++ name2 ++ ", elfelejt támadni."
  "RegeneracioCheck" -> name1 ++ " a regeneráció erejét használja és 3 pontot gyógyul."
  "HumorCheck" -> name1 ++ " mond egy viccet: vicc"
  "FurfangossagCheck" -> name1 ++ " furfangos kedvében van, és kicseréli az életpontjait az ellenfellel."
  "MuveszlelekCheck" -> name1 ++ " művészlélek elkezd gondolkodni valamit és elfelejt támadni."
  "SettenkedesCheck" -> name1 ++ " odasettenkedik " ++ name2 ++ " mellé és kétszer megtámadja."
  "TuzokadasCheck" -> name1 ++ " összegyüjti az erejét és tűzet okád!"
  "MeggyozoeroCheck" -> name1 ++ " annyira jól érvel, hogy meggyőzi ellenfelét hogy tulajdonképpen ő nyert."
  "DiplomaciaCheck" -> name1 ++ " diplomatikusan elintézi, hogy újból kezdjék a meccset."
  _ -> name1 ++ " ismeretlen hatás: \"" ++ eff ++ "\""

toString : FightLog -> String
toString fl = case fl of
  StartFight _ _ _ _ -> "Kezdődik a harc!"
  Damage n1 dmg1 _ eff1 n2 dmg2 _ eff2 ->
    let
      tamadoEffektek = List.map (describeEffect n1 n2) eff1
      vedekezoEffektetkFurfangossakNelkul =
        eff2
        |> List.filter (\e -> e /= "FurfangossagCheck")
        |> List.map (describeEffect n2 n1)

      vedekezoFurfangossagCheck =
        eff2
        |> List.filter (\e -> e == "FurfangossagCheck")
        |> List.map (describeEffect n2 n1)
        |> String.concat
    in
      String.join
        "\n"
        (tamadoEffektek ++ vedekezoEffektetkFurfangossakNelkul)
        ++ " " ++ n1 ++ " megcökkenti " ++ n2 ++ "-t, es sebezi "
        ++ String.fromInt dmg2 ++ " ponttal. "
        ++ (if dmg1 > 0 then " De " ++ n2 ++ " nem hagyja magát és vissza cökkenti " ++ n1 ++ "-t, " ++ String.fromInt dmg1 ++ " ponttal sebezve. " else "")
        ++ vedekezoFurfangossagCheck
  Win n -> n ++ " nyert!"
  Effect eff -> eff ++ " történt!"

type alias Nev = String
type alias HP = Int
type alias DMG = Int
type alias Effect = String

type FightLog
  = StartFight Nev HP Nev HP
  | Win Nev
  | Damage Nev DMG HP (List Effect) Nev DMG HP (List Effect)
  | Effect Effect

decoder : Decoder (List FightLog)
decoder = Decode.list fightLogDecoder

fightLogDecoder : Decoder FightLog
fightLogDecoder =
  Decode.field "type" Decode.string
  |> Decode.andThen (\type_ ->
    case type_ of
      "win" ->
        Decode.map
          Win
          (Decode.field "winner" Decode.string)
      "eff" ->
        Decode.map
          Effect
          (Decode.field "effect" Decode.string)
      "start" ->
        Decode.map4
          StartFight
          (Decode.field "nameA" Decode.string)
          (Decode.field "hpA" Decode.int)
          (Decode.field "nameB" Decode.string)
          (Decode.field "hpB" Decode.int)
      "damage" ->
        Decode.map8
          Damage
          (Decode.field "nameA" Decode.string)
          (Decode.field "dmgA" Decode.int)
          (Decode.field "hpA" Decode.int)
          (Decode.field "effsA" <| Decode.list (Decode.string))
          (Decode.field "nameB" Decode.string)
          (Decode.field "dmgB" Decode.int)
          (Decode.field "hpB" Decode.int)
          (Decode.field "effsB" <| Decode.list (Decode.string))
      other -> Decode.fail <| "I don't recognize this type: " ++ other
  )
