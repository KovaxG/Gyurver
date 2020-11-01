module Settings exposing (path, version)

debug : Bool
debug = False

path : String
path =
  if debug
  then "http://localhost:8080"
  else "http://totallysafelink.xyz"

version : String
version = "0.4.3"