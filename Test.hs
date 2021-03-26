{-# LANGUAGE NamedFieldPuns #-}

import Data.Function ((&))
import Endpoints (Endpoint(..), parseEndpoint)

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
  [ test "landing page" $ parseEndpoint "GET /" === GetLandingPage
  , test "CV" $ parseEndpoint "GET /cv" === GetCV
  , test "favicon" $ parseEndpoint "GET /favicon.ico" === GetFavicon

  , test "articles page EN" $ parseEndpoint "GET /articles" === GetArticlesPage
  , test "articles page HU" $ parseEndpoint "GET /cikkek" === GetArticlesPage
  , test "articles page RO" $ parseEndpoint "GET /articole" === GetArticlesPage

  , test "2020 cokk page" $ parseEndpoint "GET /cokk2020" === GetCokk2020Page
  , test "2021 cokk page" $ parseEndpoint "GET /cokk2021" === GetCokk2021Page

  , test "2020 cokk results page EN" $ parseEndpoint "GET /cokk2020/results" === GetCokk2020ResultsPage
  , test "2020 cokk results page HU" $ parseEndpoint "GET /cokk2020/eredmenyek" === GetCokk2020ResultsPage
  , test "2020 cokk results page RO" $ parseEndpoint "GET /cokk2020/rezultate" === GetCokk2020ResultsPage

  , test "2020 cokk API" $ parseEndpoint "GET /api/cokk2020" === GetCokk2020JSON

  , test "2021 cokk login API" $ parseEndpoint "POST /api/cokk2021/login" === PostCokk2021Login
  , test "2021 cokk register API" $ parseEndpoint "POST /api/cokk2021/register" === PostCokk2021Register
  , test "2021 cokk eggs API" $ parseEndpoint "GET /api/cokk2021/resztvevok" === GetCokk2021Participants
  , test "2021 cokk eggs API for user" $ parseEndpoint "POST /api/cokk2021/resztvevok" === PostCokk2021ParticipantsForUser
  , test "2021 cokk watering API" $ parseEndpoint "POST /api/cokk2021/water" === PostCokk2021Water
  , test "2021 cokk dashboard refresh API" $ parseEndpoint "POST /api/cokk2021/dashboard" === PostCokk2021DashboardRefresh
  , test "2021 cokk increase skill API" $ parseEndpoint "POST /api/cokk2021/skills/inc" === PostCokk2021IncSkill
  , test "2021 cokk change egg API" $ parseEndpoint "POST /api/cokk2021/update/eggname" === PostCokk2021ChangeEggname
  , test "2021 cokk items API" $ parseEndpoint "GET /api/cokk2021/items" === GetCokk2021Items
  , test "2021 buy cokk item" $ parseEndpoint "POST /api/cokk2021/items/buy" === PostCokk2021BuyItem
  , test "2021 equip cokk item" $ parseEndpoint "POST /api/cokk2021/items/equip" === PostCokk2021EquipItem

  , test "videos page EN" $ parseEndpoint "GET /videos" === GetVideosPage
  , test "videos page HU" $ parseEndpoint "GET /videok" === GetVideosPage
  , test "videos page RO" $ parseEndpoint "GET /videouri" === GetVideosPage

  , test "videos API EN" $ parseEndpoint "GET /api/videos" === GetVideosJSON
  , test "videos API HU" $ parseEndpoint "GET /api/videok" === GetVideosJSON
  , test "videos API RO" $ parseEndpoint "GET /api/videouri" === GetVideosJSON

  , test "correct video API GET" $ parseEndpoint "GET /api/video/42" === GetVideoJSON 42
  , test "alpha videos API GET" $ parseEndpoint "GET /api/video/asdf" === Other "GET /api/video/asdf"
  , test "missing videos API GET" $ parseEndpoint "GET /api/video/" === Other "GET /api/video/"

  , test "correct video API POST" $ parseEndpoint "POST /api/video/42" === PostVideoJSON 42
  , test "alpha videos API POST" $ parseEndpoint "POST /api/video/asdf" === Other "POST /api/video/asdf"
  , test "missing videos API POST" $ parseEndpoint "POST /api/video/" === Other "POST /api/video/"

  , test "correct video OPTIONS" $ parseEndpoint "OPTIONS /api/video/42" === OptionsVideoJSON 42
  , test "alpha videos OPTIONS" $ parseEndpoint "OPTIONS /api/video/asdf" === Other "OPTIONS /api/video/asdf"
  , test "missing videos OPTIONS" $ parseEndpoint "OPTIONS /api/video/" === Other "OPTIONS /api/video/"

  , test "correct video DELETE" $ parseEndpoint "DELETE /api/video/42" === DeleteVideoJSON 42
  , test "alpha videos DELETE" $ parseEndpoint "DELETE /api/video/asdf" === Other "DELETE /api/video/asdf"
  , test "missing videos DELETE" $ parseEndpoint "DELETE /api/video/" === Other "DELETE /api/video/"

  , test "video add page EN" $ parseEndpoint "GET /videos/new" === GetVideosAddPage
  , test "video add page HU" $ parseEndpoint "GET /videok/uj" === GetVideosAddPage
  , test "video add page RO" $ parseEndpoint "GET /videouri/nou" === GetVideosAddPage

  , test "posting new video EN" $ parseEndpoint "POST /api/videos" === PostVideo
  , test "posting new video HU" $ parseEndpoint "POST /api/videok" === PostVideo
  , test "posting new video RO" $ parseEndpoint "POST /api/videouri" === PostVideo

  , test "video options HU" $ parseEndpoint "OPTIONS /api/videok" === OptionsVideo
  , test "video options RO" $ parseEndpoint "OPTIONS /api/videouri" === OptionsVideo
  , test "video options EN" $ parseEndpoint "OPTIONS /api/videos" === OptionsVideo

  , test "requesting pdf resource: test.pdf" $ parseEndpoint "GET /res/test.pdf" === GetResource "test.pdf"
  , test "requesting pdf resource: test_underscore.pdf" $ parseEndpoint "GET /res/test_underscore.pdf" === GetResource "test_underscore.pdf"
  ]

main :: IO ()
main = putStrLn $ runTests tests

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f = fmap (\a -> (a, f a))
