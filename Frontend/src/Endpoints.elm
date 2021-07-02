module Endpoints exposing (..)

-- This file is duplicated in Gyurver Endpoints.hs
landingPage = "/"


articlesPageEN = "/articles"
articlesPageHU = "/cikkek"
articlesPageRO = "/articole"


blogPage = "/blog"

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
cokk2021ResultsPageEN = "/cokk2021/results"

cokk2020ResultsPageEN = "/cokk2020/results"
cokk2020ResultsPageHU = "/cokk2020/eredmenyek"
cokk2020ResultsPageRO = "/cokk2020/rezultate"

cokk2020Json = "/api/cokk2020"

cokk2021RegisterJson = "/api/cokk2021/register"
cokk2021LoginJson = "/api/cokk2021/login"
cokk2021ParticipantsJson = "/api/cokk2021/resztvevok"
cokk2021DashboardJson = "/api/cokk2021/dashboard"
cokk2021IncSkillJson =  "/api/cokk2021/skills/inc"
cokk2021updateEggNameJson = "/api/cokk2021/update/eggname"
cokk2021Water = "/api/cokk2021/water"
cokk2021Items = "/api/cokk2021/items"
cokk2021BuyItemJson = "/api/cokk2021/items/buy"
cokk2021EquipItemJson = "/api/cokk2021/items/equip"
cokk2021FightJson = "/api/cokk2021/fight"
suggestionBox = "/api/suggestionbox"

cv = "/cv"
