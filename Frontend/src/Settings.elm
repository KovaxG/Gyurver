module Settings exposing (path)

debug : Bool
debug = False

path : String
path = 
  if debug 
  then "http://localhost:8080"
  else "http://totallysafelink.xyz"
