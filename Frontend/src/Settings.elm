module Settings exposing (path, version, landingPage, cv, cokkPage, cokkResultsPage, articlesPage, videosPage, videoAddPage, cokkJson, videosJson)

debug : Bool
debug = False

path : String
path =
  if debug
  then "http://localhost:8080"
  else "http://totallysafelink.xyz"

version : String
version = "0.4.3"


-- TODO extract to a new file
-- TODO also add trilingual endpoints
landingPage = "/"
cv = "/cv"

cokkPage = "/cokk"
cokkResultsPage = "/cokk/eredmenyek"
cokkJson = "/api/cokk"

articlesPage = "/articles"
videosPage = "/videos"
videoAddPage = "/videos/add"
videosJson = "/api/videos"
