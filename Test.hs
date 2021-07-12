{-# LANGUAGE NamedFieldPuns #-}

import Data.Function ((&))
import Endpoints (Endpoint(..), Operation(..), parse)

data Assertion a = Equality a a deriving (Show)

(===) :: Eq a => a -> a -> Assertion a
a === b = Equality a b

data Test a = Test
  { description :: String
  , assertion :: Assertion a
  }

data Result = Passed | Failed deriving (Eq)

test :: String -> Assertion a -> Test a
test description assertion = Test { description, assertion }

runTest :: Eq a => Test a -> Result
runTest Test { assertion } = case assertion of
  Equality a b -> if a == b then Passed else Failed

runTests :: (Eq a, Show a) => [Test a] -> String
runTests tests = unlines (passed ++ [""] ++ failed) ++ "\n" ++ lastLine ++ normalText
  where
    results = zipMap runTest tests
    passed = results & filter ((==Passed) . snd) & map ((++) greenText . description . fst)
    failed = results & filter ((==Failed) . snd) & map (\(t, _) -> (++ " -> " ++ show (assertion t)) $ (++) redText $ description $ t)
    lastColor = if null failed then greenText else redText
    lastLine = lastColor ++ "Passed: " ++ show (length passed) ++ " Failed: " ++ show (length failed)

normalText :: String
normalText = "\ESC[00m"

greenText :: String
greenText = "\ESC[01;32m"

redText :: String
redText = "\ESC[01;31m"

tests :: [Test Endpoint]
tests =
  [ test "landing page" $ parse "GET /" === GetLandingPage
  , test "CV" $ parse "GET /cv" === GetCV
  , test "favicon" $ parse "GET /favicon.ico" === GetFavicon

  , test "articles page EN" $ parse "GET /articles" === GetArticlesPage
  , test "articles page HU" $ parse "GET /cikkek" === GetArticlesPage
  , test "articles page RO" $ parse "GET /articole" === GetArticlesPage

  , test "2020 cokk page" $ parse "GET /cokk2020" === GetCokk2020Page
  , test "2021 cokk page" $ parse "GET /cokk2021" === GetCokk2021Page

  , test "2020 cokk results page EN" $ parse "GET /cokk2020/results" === GetCokk2020ResultsPage
  , test "2020 cokk results page HU" $ parse "GET /cokk2020/eredmenyek" === GetCokk2020ResultsPage
  , test "2020 cokk results page RO" $ parse "GET /cokk2020/rezultate" === GetCokk2020ResultsPage

  , test "2021 cokk results page EN" $ parse "GET /cokk2021/results" === GetCokk2021ResultsPage

  , test "2020 cokk API" $ parse "GET /api/cokk2020" === GetCokk2020JSON

  , test "2021 cokk login API" $ parse "POST /api/cokk2021/login" === PostCokk2021Login
  , test "2021 cokk register API" $ parse "POST /api/cokk2021/register" === PostCokk2021Register
  , test "2021 cokk eggs API" $ parse "GET /api/cokk2021/resztvevok" === GetCokk2021Participants
  , test "2021 cokk eggs API for user" $ parse "POST /api/cokk2021/resztvevok" === PostCokk2021ParticipantsForUser
  , test "2021 cokk watering API" $ parse "POST /api/cokk2021/water" === PostCokk2021Water
  , test "2021 cokk dashboard refresh API" $ parse "POST /api/cokk2021/dashboard" === PostCokk2021DashboardRefresh
  , test "2021 cokk increase skill API" $ parse "POST /api/cokk2021/skills/inc" === PostCokk2021IncSkill
  , test "2021 cokk change egg API" $ parse "POST /api/cokk2021/update/eggname" === PostCokk2021ChangeEggname
  , test "2021 cokk items API" $ parse "GET /api/cokk2021/items" === GetCokk2021Items
  , test "2021 buy cokk item" $ parse "POST /api/cokk2021/items/buy" === PostCokk2021BuyItem
  , test "2021 equip cokk item" $ parse "POST /api/cokk2021/items/equip" === PostCokk2021EquipItem
  , test "2021 fight cokk" $ parse "POST /api/cokk2021/fight" === PostCokk2021Fight

  , test "suggestion box" $ parse "POST /api/suggestionbox" === PostSuggestion

  , test "videos page EN" $ parse "GET /videos" === GetVideosPage
  , test "videos page HU" $ parse "GET /videok" === GetVideosPage
  , test "videos page RO" $ parse "GET /videouri" === GetVideosPage

  , test "blog page" $ parse "GET /blog" === GetBlogPage
  , test "specific blog page" $ parse "GET /blog/42" === GetBlogItemPage 42
  , test "specific blog page" $ parse "GET /blog/bla" === Other "GET /blog/bla"

  , test "get blog items" $ parse "GET /api/blog/items" === GetBlogItemsJSON
  , test "correct blog page" $ parse "GET /api/blog/1" === GetBlogJSON 1
  , test "incorrect blog page" $ parse "GET /api/blog/a" === Other "GET /api/blog/a"

  , test "videos API EN" $ parse "GET /api/videos" === GetVideosJSON
  , test "videos API HU" $ parse "GET /api/videok" === GetVideosJSON
  , test "videos API RO" $ parse "GET /api/videouri" === GetVideosJSON

  , test "correct video API GET" $ parse "GET /api/video/42" === GetVideoJSON 42
  , test "alpha videos API GET" $ parse "GET /api/video/asdf" === Other "GET /api/video/asdf"
  , test "missing videos API GET" $ parse "GET /api/video/" === Other "GET /api/video/"

  , test "correct video API POST" $ parse "POST /api/video/42" === PostVideoJSON 42
  , test "alpha videos API POST" $ parse "POST /api/video/asdf" === Other "POST /api/video/asdf"
  , test "missing videos API POST" $ parse "POST /api/video/" === Other "POST /api/video/"

  , test "correct video OPTIONS" $ parse "OPTIONS /api/video/42" === OptionsVideoJSON 42
  , test "alpha videos OPTIONS" $ parse "OPTIONS /api/video/asdf" === Other "OPTIONS /api/video/asdf"
  , test "missing videos OPTIONS" $ parse "OPTIONS /api/video/" === Other "OPTIONS /api/video/"

  , test "correct video DELETE" $ parse "DELETE /api/video/42" === DeleteVideoJSON 42
  , test "alpha videos DELETE" $ parse "DELETE /api/video/asdf" === Other "DELETE /api/video/asdf"
  , test "missing videos DELETE" $ parse "DELETE /api/video/" === Other "DELETE /api/video/"

  , test "video add page EN" $ parse "GET /videos/new" === GetVideosAddPage
  , test "video add page HU" $ parse "GET /videok/uj" === GetVideosAddPage
  , test "video add page RO" $ parse "GET /videouri/nou" === GetVideosAddPage

  , test "posting new video EN" $ parse "POST /api/videos" === PostVideo
  , test "posting new video HU" $ parse "POST /api/videok" === PostVideo
  , test "posting new video RO" $ parse "POST /api/videouri" === PostVideo

  , test "posting a new film API POST" $ parse "POST /api/films" === Film Insert
  , test "marking a film as watched API POST" $ parse "PUT /api/films" === Film Modify
  , test "get a list of films API GET" $ parse "GET /api/films" === Film Obtain
  , test "delete a film API DELETE" $ parse "DELETE /api/films" === Film Delete

  , test "video options HU" $ parse "OPTIONS /api/videok" === OptionsVideo
  , test "video options RO" $ parse "OPTIONS /api/videouri" === OptionsVideo
  , test "video options EN" $ parse "OPTIONS /api/videos" === OptionsVideo

  , test "requesting pdf resource: test.pdf" $ parse "GET /res/test.pdf" === GetResource "test.pdf"
  , test "requesting pdf resource: test_underscore.pdf" $ parse "GET /res/test_underscore.pdf" === GetResource "test_underscore.pdf"
  ]

main :: IO ()
main = putStrLn $ runTests tests

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f = fmap (\a -> (a, f a))
