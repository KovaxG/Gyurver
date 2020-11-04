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
  , test "articles page" $ parseEndpoint "GET /articles" === GetArticlesPage
  , test "cokk API" $ parseEndpoint "GET /cokk/list" === GetCokkJSON
  , test "videos page" $ parseEndpoint "GET /vids" === GetVideosPage
  , test "videos API" $ parseEndpoint "GET /api/vids" === GetVideosJSON
  , test "cokk results page" $ parseEndpoint "GET /cokk/eredmeny" === GetCokkResultsPage
  , test "cokk page" $ parseEndpoint "GET /cokk" === GetCokkPage
  , test "video add page" $ parseEndpoint "GET /vids/add" === GetVideosAddPage
  , test "requesting pdf resource" $ parseEndpoint "GET /res/test.pdf" === GetResource "test.pdf"
  , test "posting new video" $ parseEndpoint "POST /api/vids" === PostVideo
  , test "video options" $ parseEndpoint "OPTIONS /api/vids" === OptionsVideo
  ]

main :: IO ()
main = putStrLn $ runTests tests

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f as = fmap (\a -> (a, f a)) as