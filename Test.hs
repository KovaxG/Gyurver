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
    lastColor = if length failed == 0 then greenText else redText
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


  , test "cokk page" $ parseEndpoint "GET /cokk" === GetCokkPage

  , test "cokk results page EN" $ parseEndpoint "GET /cokk/results" === GetCokkResultsPage
  , test "cokk results page HU" $ parseEndpoint "GET /cokk/eredmenyek" === GetCokkResultsPage
  , test "cokk results page RO" $ parseEndpoint "GET /cokk/rezultate" === GetCokkResultsPage

  , test "cokk API" $ parseEndpoint "GET /api/cokk" === GetCokkJSON


  , test "videos page EN" $ parseEndpoint "GET /videos" === GetVideosPage
  , test "videos page HU" $ parseEndpoint "GET /videok" === GetVideosPage
  , test "videos page RO" $ parseEndpoint "GET /videouri" === GetVideosPage

  , test "videos API EN" $ parseEndpoint "GET /api/videos" === GetVideosJSON
  , test "videos API HU" $ parseEndpoint "GET /api/videok" === GetVideosJSON
  , test "videos API RO" $ parseEndpoint "GET /api/videouri" === GetVideosJSON

  , test "video add page EN" $ parseEndpoint "GET /videos/add" === GetVideosAddPage
  , test "video add page HU" $ parseEndpoint "GET /videok/add" === GetVideosAddPage
  , test "video add page RO" $ parseEndpoint "GET /videouri/add" === GetVideosAddPage

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
zipMap f as = fmap (\a -> (a, f a)) as