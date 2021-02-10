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


cokk2020Page = "/cokk2020"
cokk2021Page = "/cokk2021"

cokk2020ResultsPageEN = "/cokk2020/results"
cokk2020ResultsPageHU = "/cokk2020/eredmenyek"
cokk2020ResultsPageRO = "/cokk2020/rezultate"

cokk2020Json = "/api/cokk2020"

cokk2021RegisterJson = "/api/cokk2021/register"
cokk2021LoginJson = "/api/cokk2021/login"

cv = "/cv"
