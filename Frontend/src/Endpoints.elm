module Endpoints exposing (..)

-- This file is duplicated in Gyurver Endpoints.hs
landingPage = "/"


articlesPageEN = "/articles"
articlesPageHU = "/cikkek"
articlesPageRO = "/articole"


videosPageEN = "/videos"
videosPageHU = "/videok"
videosPageRO = "/videouri"


videoAddPageEN = "/videos/new"
videoAddPageHU = "/videok/uj"
videoAddPageRO = "/videouri/nou"


videosJsonEN = "/api/videos"
videosJsonHU = "/api/videok"
videosJsonRO = "/api/videouri"


videoJson nr = "/api/video/" ++ String.fromInt nr


cokkPage = "/cokk"


cokkResultsPageEN = "/cokk/results"
cokkResultsPageHU = "/cokk/eredmenyek"
cokkResultsPageRO = "/cokk/rezultate"

cokkJson = "/api/cokk"


cv = "/cv"
