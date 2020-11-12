module Types.Result exposing (..)

merge : Result a a -> a
merge r = case r of
  Ok a -> a
  Err a -> a
