{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Endpoints (Endpoint(..), Operation(..), RightsOperation(..), parse)
import           Types.Blog (Blog(..), Metadata(..), Reference(..), Section(..), parseGyurblog)
import           Types.Date (Date(..))
import           Types.Language (Language(..))

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

runTests :: (Eq a, Show a) => String -> [Test a] -> String
runTests descr tests = "------------\n" ++ unlines (passed ++ [""] ++ failed) ++ "\n" ++ descr ++ ": " ++ lastLine ++ normalText
  where
    results = zipMap runTest tests
    passed = results & filter ((==Passed) . snd) & map ((++) greenText . description . fst)
    failed = results & filter ((==Failed) . snd) & map (\(t, _) -> (++ " -> " ++ show (assertion t)) $ (++) redText $ description t)
    lastColor = if null failed then greenText else redText
    totalPassed = length passed
    totalFailed = length failed
    lastLine = lastColor ++ "Total: " ++ show (totalPassed + totalFailed) ++ " Passed: " ++ show totalPassed ++ " Failed: " ++ show totalFailed

normalText :: String
normalText = "\ESC[00m"

greenText :: String
greenText = "\ESC[01;32m"

redText :: String
redText = "\ESC[01;31m"

endpointTests :: [Test Endpoint]
endpointTests =
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
  , test "get films page" $ parse "GET /films" === GetFilmsPage

  , test "video options HU" $ parse "OPTIONS /api/videok" === OptionsVideo
  , test "video options RO" $ parse "OPTIONS /api/videouri" === OptionsVideo
  , test "video options EN" $ parse "OPTIONS /api/videos" === OptionsVideo

  , test "get rights" $ parse "GET /api/rights" === Rights GetAll
  , test "add secret" $ parse "POST /api/rights" === Rights AddSecret

  , test "requesting pdf resource: test.pdf" $ parse "GET /res/test.pdf" === GetResource "test.pdf"
  , test "requesting pdf resource: test_underscore.pdf" $ parse "GET /res/test_underscore.pdf" === GetResource "test_underscore.pdf"
  ]

gyurblogParseTests :: [Test (Either String Blog)]
gyurblogParseTests =
  [ test "Empty file" $
    parseGyurblog 0 blankFile === Left "Did not find title."

  , test "Blank title" $
    parseGyurblog 0 blankTitle === Left "Title can't be blank!"

  , test "Single line" $
    parseGyurblog 0 noDate === Left "Did not find date."

  , test "Invalid date" $
    parseGyurblog 0 invalidDate === Left "Invalid Date!"

  , test "Minimum" $
    parseGyurblog 0 minimumFile === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , metadata = defaultMetadata { languages = [EN] }
        }
    )

  , test "Language" $
    parseGyurblog 0 languageFile === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , metadata = defaultMetadata { languages = [HU, RO, DE] }
        }
    )

  , test "Invalid Language" $
    parseGyurblog 0 invalidLanguageFile === Left "Invalid Language!"

  , test "Topics" $
    parseGyurblog 0 topicsFile === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , metadata = defaultMetadata { languages = [EN], topics = ["tag1", "tag2", "tag3"] }
        }
    )

  , test "Full Metadata" $
    parseGyurblog 0 fullMetadata === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , metadata = defaultMetadata { languages = [HU, RO, DE], topics = ["tag1", "tag2", "tag3"] }
        }
    )

  , test "Single Metadata" $
    parseGyurblog 0 singleMetadata === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , metadata = defaultMetadata { languages = [HU], topics = ["tag"] }
        }
    )

  , test "Single Intro" $
    parseGyurblog 0 singleIntro === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , intro = "This is the intro."
        , metadata = defaultMetadata { languages = [EN]}
        }
    )

  , test "Double Intro" $
    parseGyurblog 0 doubleIntro === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , intro = "HelloWorld"
        , metadata = defaultMetadata { languages = [EN]}
        }
    )

  , test "Invalid Intro" $
    parseGyurblog 0 badIntro === Right (
      defaultBlog
        { title = "This is the title"
        , date = Date 2021 8 7
        , sections = [Paragraph "(Hello"]
        , metadata = defaultMetadata { languages = [EN]}
        }
    )

  , test "Single Reference" $
    parseGyurblog 0 singleRef === Right (
      defaultBlog
        { title = "Title"
        , date = Date 2021 8 7
        , sections = [Paragraph "Look, a ref: [1]"]
        , references = [Ref 1 "My site" "www.totallysafelink.xyz"]
        , metadata = defaultMetadata { languages = [EN]}
        }
    )

  , test "Multi Reference" $
    parseGyurblog 0 multiRef === Right (
      defaultBlog
        { title = "Title"
        , date = Date 2021 8 7
        , sections = [Paragraph "Look, refs: [1][2][3][4][5]"]
        , references =
          [ Ref 1 "My site" "www.totallysafelink.xyz"
          , Ref 2 "My site" "www.totallysafelink.xyz"
          , Ref 3 "My site" "www.totallysafelink.xyz"
          , Ref 4 "My site" "www.totallysafelink.xyz"
          , Ref 5 "My site" "www.totallysafelink.xyz"
          ]
        , metadata = defaultMetadata { languages = [EN]}
        }
    )

  , test "Referencing non existent ref" $
    parseGyurblog 0 invalidRef === Left "Referencing non existent ref: [2]"

  , test "Having extra references" $
    parseGyurblog 0 extraRef === Left "Not referenced in the text: [2]"
  ]

blankFile :: Text
blankFile = ""

blankTitle :: Text
blankTitle = "title: "

noDate :: Text
noDate = "title: Title"

invalidDate :: Text
invalidDate = Text.unlines
  [ "title: This is the title"
  , "date: This is the date"
  ]

minimumFile :: Text
minimumFile = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  ]

languageFile :: Text
languageFile = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "lang: HU, RO, DE"
  ]

invalidLanguageFile :: Text
invalidLanguageFile = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "lang: lol"
  ]

topicsFile :: Text
topicsFile = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "tags: tag1, tag2, tag3"
  ]

fullMetadata :: Text
fullMetadata = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "lang: HU, RO, DE"
  , "tags: tag1, tag2, tag3"
  ]

singleMetadata :: Text
singleMetadata = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "lang: HU"
  , "tags: tag"
  ]

singleIntro :: Text
singleIntro = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "(This is the intro.)"
  ]

doubleIntro :: Text
doubleIntro = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "(Hello)"
  , "(World)"
  ]

badIntro :: Text
badIntro = Text.unlines
  [ "title: This is the title"
  , "date: 2021-08-07"
  , "(Hello"
  ]

singleRef :: Text
singleRef = Text.unlines
  [ "title: Title"
  , "date: 2021-08-07"
  , "Look, a ref: [1]"
  , "[1] My site (www.totallysafelink.xyz)"
  ]

multiRef :: Text
multiRef = Text.unlines
  [ "title: Title"
  , "date: 2021-08-07"
  , "Look, refs: [1][2][3][4][5]"
  , "[1] My site (www.totallysafelink.xyz)"
  , "[2] My site (www.totallysafelink.xyz)"
  , "[3] My site (www.totallysafelink.xyz)"
  , "[4] My site (www.totallysafelink.xyz)"
  , "[5] My site (www.totallysafelink.xyz)"
  ]

invalidRef :: Text
invalidRef = Text.unlines
  [ "title: Title"
  , "date: 2021-08-07"
  , "Ok, so look. This is a valid ref [1], however some are not so valid."
  , "For example, this is not a valid ref: [2]"
  , "[1] My site (www.totallysafelink.xyz)"
  ]

extraRef :: Text
extraRef = Text.unlines
  [ "title: Title"
  , "date: 2021-08-07"
  , "Ok, so look. This is a valid ref [1], however some are not so valid."
  , "[1] My site (www.totallysafelink.xyz)"
  , "[2] My site (www.totallysafelink.xyz)"
  ]

defaultBlog :: Blog
defaultBlog = Blog
  { identifier = 0
  , title = ""
  , date = Date 2020 1 1
  , intro  = ""
  , sections  = []
  , references = []
  , metadata = defaultMetadata
  }

defaultMetadata :: Metadata
defaultMetadata = Metadata { languages = [], topics = [] }

main :: IO ()
main = do
  putStrLn $ runTests "Endpoint tests" endpointTests
  putStrLn $ runTests "Gyurblog tests" gyurblogParseTests

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f = fmap (\a -> (a, f a))
