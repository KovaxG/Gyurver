module Settings exposing (path, version, cokk2021)

import Types.EventState exposing (EventState(..))

-- TODO this should be false when releasing!!!
{- Elcsesztem:
  * 2021.03.18 - 0.6.0
  * 2021.04.05 - 0.6.4
  * 2021.04.16 - 0.6.8
-}
debug : Bool
debug = True

cokk2021 : EventState
cokk2021 = Blocked

path : String
path =
  if debug
  then "http://localhost:8080"
  else "http://totallysafelink.xyz"

version : String
version = "0.6.10"
