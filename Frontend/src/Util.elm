module Util exposing (..)

import Http exposing (Error(..))
import Types.Result as Result
import List.Extra as List

mapIf : (a -> Bool) -> (a -> a) -> List a -> List a
mapIf p f = List.map (\a -> if p a then f a else a)

showError : Error -> String
showError error = case error of
  BadUrl str -> str
  Timeout -> "Request timed out. Check out the server, it might be overloaded."
  NetworkError -> "Network Error. Lol the description says that it means the user turned off their wifi, went in a cave, etc. :))"
  BadStatus code ->
    if code == 401
    then "Téves jelszó vagy felhasználónév. Vigyázat mert számítanak a kis-nagybetűk a névben is!"
    else "Bad Status with code: " ++ String.fromInt code
  BadBody str -> "Bad Body: " ++ str

processMessage : (a -> msg) -> (String -> msg) -> Result Error a -> msg
processMessage success fail result =
  result
  |> Result.map success
  |> Result.mapError (fail << showError)
  |> Result.merge

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

removeFinalDigits : String -> String
removeFinalDigits url =
  url
  |> String.toList
  |> List.dropWhileRight Char.isDigit
  |> String.fromList
