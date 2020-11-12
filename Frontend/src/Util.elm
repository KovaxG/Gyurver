module Util exposing (..)

mapIf : (a -> Bool) -> (a -> a) -> List a -> List a
mapIf p f = List.map (\a -> if p a then f a else a)