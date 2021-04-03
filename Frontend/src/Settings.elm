module Settings exposing (path, version)

-- TODO this should be false when releasing!!!
{- Elcsesztem:
  * 2021.03.18 - 0.6.0
-}
debug : Bool
debug = False

path : String
path =
  if debug
  then "http://localhost:8080"
  else "http://totallysafelink.xyz"

version : String
version = "0.6.2"
