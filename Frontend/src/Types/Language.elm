module Types.Language exposing (..)

import Json.Decode as Decode exposing (Decoder)

type Language = EN | RO | HU | DE

flag : Language -> String
flag lang = case lang of
  EN -> "🏴󠁧󠁢󠁥󠁮󠁧󠁿"
  HU -> "🇭🇺"
  RO -> "🇷🇴"
  DE -> "🇩🇪"

decoder : Decoder Language
decoder =
  Decode.string
  |> Decode.andThen (\s ->
    case s of
      "EN" -> Decode.succeed EN
      "RO" -> Decode.succeed RO
      "HU" -> Decode.succeed HU
      "DE" -> Decode.succeed DE
      other -> Decode.fail (other ++ " is not a language I recognize!")
  )
