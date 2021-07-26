module Endpoints exposing (..)

import Url
import Url.Parser as Parser exposing (Parser, (</>))

-- This file is duplicated in Gyurver Endpoints.hs, not sure what to do about this fact

type Endpoint
  = LandingPage
  | Cokk2020Page
  | BlogItemPage Int

parse : String -> Maybe Endpoint
parse path =
  let fullUrl = "http://fake.com" ++ path
      actualParser : Parser (Endpoint -> a) a
      actualParser =
        Parser.oneOf
          [ Parser.map LandingPage Parser.top
          , Parser.map Cokk2020Page (Parser.s "cokk2020")
          , Parser.map BlogItemPage (Parser.s "blog" </> Parser.int)
          ]
  in fullUrl
     |> Url.fromString
     |> Maybe.andThen (Parser.parse actualParser)

  -- , Parser.keyword Endpoints.cokk2020ResultsPageEN
  --     |> Parser.map (\_ -> Cokkolo2020.Results.init |> liftModelCmd CokkoloResults2020 CokkoloResults2020Msg model)
  -- , Parser.keyword Endpoints.cokk2020ResultsPageHU
  --     |> Parser.map (\_ -> Cokkolo2020.Results.init |> liftModelCmd CokkoloResults2020 CokkoloResults2020Msg model)
  -- , Parser.keyword Endpoints.cokk2020ResultsPageRO
  --     |> Parser.map (\_ -> Cokkolo2020.Results.init |> liftModelCmd CokkoloResults2020 CokkoloResults2020Msg model)
  -- , Parser.keyword Endpoints.cokk2021Page
  --     |> Parser.map (\_ -> Cokkolo2021.Landing.init |> liftModelCmd CokkoloLanding2021 CokkoloLanding2021Msg model)
  -- , Parser.keyword Endpoints.cokk2021ResultsPageEN
  --     |> Parser.map (\_ -> Cokkolo2021.Results.init |> liftModelCmd CokkoloResults2021 CokkoloResults2021Msg model)
  -- , Parser.keyword Endpoints.articlesPageEN
  --     |> Parser.map (\_ -> Articles.init |> liftModelCmd Articles ArticlesMsg  model)
  -- , Parser.keyword Endpoints.articlesPageHU
  --     |> Parser.map (\_ -> Articles.init |> liftModelCmd Articles ArticlesMsg  model)
  -- , Parser.keyword Endpoints.articlesPageRO
  --     |> Parser.map (\_ -> Articles.init |> liftModelCmd Articles ArticlesMsg  model)
  -- , Parser.keyword Endpoints.videosPageEN
  --     |> Parser.map (\_ -> VideoList.init |> liftModelCmd VideoList VideoListMsg model)
  -- , Parser.keyword Endpoints.videosPageHU
  --     |> Parser.map (\_ -> VideoList.init |> liftModelCmd VideoList VideoListMsg model)
  -- , Parser.keyword Endpoints.videosPageRO
  --     |> Parser.map (\_ -> VideoList.init |> liftModelCmd VideoList VideoListMsg model)
  -- , Parser.keyword Endpoints.videoAddPageEN
  --     |> Parser.map (\_ -> VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model)
  -- , Parser.keyword Endpoints.videoAddPageHU
  --     |> Parser.map (\_ -> VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model)
  -- , Parser.keyword Endpoints.videoAddPageRO
  --     |> Parser.map (\_ -> VideoAdd.init |> liftModelCmd VideoAdd VideoAddMsg model)
  -- , Parser.keyword Endpoints.blogPage
  --   |> Parser.map (\_ -> BlogList.init |> liftModelCmd BlogList BlogListMsg model)
  -- ]

show : Endpoint -> String
show ep = case ep of
  LandingPage -> "/"
  Cokk2020Page -> "/cokk2020"
  BlogItemPage nr -> "/blog/" ++ String.fromInt nr

articlesPageEN = "/articles"
articlesPageHU = "/cikkek"
articlesPageRO = "/articole"


blogPage = "/blog"

blogItemsJson = "/api/blog/items"
blogItemJson = "/api/blog/"

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
