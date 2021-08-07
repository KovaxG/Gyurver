{-# LANGUAGE TupleSections #-}
module Types.Blog where

import           Component.Database (DBFormat(..))
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Types.Date (Date(..))
import qualified Types.Date as Date
import           Types.Language (Language(..))
import qualified Types.Language as Language
import qualified Utils

type Topic = String
type Title = String

data Blog = Blog
  { identifier :: Int
  , title :: Title
  , date :: Date
  , intro :: String
  , sections :: [Section]
  , references :: [Reference]
  , metadata :: Metadata
  } deriving (Show, Eq)

data Section = Paragraph String deriving (Show, Eq)

data Reference = Ref Int String String deriving (Show, Eq)

data Metadata = Metadata
  { languages :: [Language]
  , topics :: [Topic]
  } deriving (Show, Eq)

showSectionASCII :: Section -> String
showSectionASCII (Paragraph body) = body ++ "\n"

showReferenceASCII :: Reference -> String
showReferenceASCII (Ref i name url) = "[" ++ show i ++ "] " ++ name ++ " - " ++ url

showASCII :: Blog -> String
showASCII blog = unlines $ mconcat
  [ [title blog]
  , ["------------"]
  , [show $ date blog]
  , [""]
  , [intro blog]
  , map showSectionASCII (sections blog)
  , [""]
  , map showReferenceASCII (references blog)
  ]

testBlog :: Blog
testBlog = Blog
  { identifier = 0
  , title = "Test Blog"
  , date = Date 2021 7 2
  , intro = "This is the intro"
  , sections =
      [ Paragraph "Hey, this is the first test blog. It is not intended to be shared, it is only intended for testing. Here is how I will refer to stuff: I like youtube [1]"
      , Paragraph "This is the second paragraph. I think I will not add newlines in paragraphs, because that way it will be much simpler for me to work with the stuff."
      ]
  , references =
      [ Ref 1 "Youtube" "www.youtube.com"
      , Ref 2 "Google" "www.google.com"
      ]
  , metadata = Metadata
    { languages = [EN]
    , topics = ["test"]
    }
  }

toJson :: Blog -> Json
toJson blog = JsonObject
  [ ("identifier", JsonNumber $ fromIntegral $ identifier blog)
  , ("title", JsonString $ title blog)
  , ("date", Date.toJson $ date blog)
  , ("intro", JsonString $ intro blog)
  , ("sections", JsonArray $ map sectionToJson $ sections blog)
  , ("references", JsonArray $ map refToJson $ references blog)
  , ("metadata", metadataToJson $ metadata blog)
  ]

sectionToJson :: Section -> Json
sectionToJson (Paragraph body) = JsonObject
  [ ("section", JsonString "paragraph")
  , ("content", JsonString body)
  ]

refToJson :: Reference -> Json
refToJson (Ref index name url) = JsonObject
  [ ("index", JsonNumber $ fromIntegral index)
  , ("name", JsonString name)
  , ("url", JsonString url)
  ]

metadataToJson :: Metadata -> Json
metadataToJson (Metadata languages topics) = JsonObject
  [ ("languages", JsonArray $ map (JsonString . show) languages)
  , ("topics", JsonArray $ map JsonString topics)
  ]

decoder :: Decoder Blog
decoder =
  Blog <$> Decoder.field "identifier" Decoder.int
       <*> Decoder.field "title" Decoder.string
       <*> Decoder.field "date" Date.decoder
       <*> Decoder.field "intro" Decoder.string
       <*> Decoder.field "sections" (Decoder.list decodeSection)
       <*> Decoder.field "references" (Decoder.list decodeReference)
       <*> Decoder.field "metadata" decodeMetadata

decodeSection :: Decoder Section
decodeSection =
  (\section content ->
    -- well since section can only be Paragraph for now I have no reason to check this
    Paragraph content
    )
    <$> Decoder.field "section" Decoder.string
    <*> Decoder.field "content" Decoder.string

decodeReference :: Decoder Reference
decodeReference =
  Ref <$> Decoder.field "index" Decoder.int
      <*> Decoder.field "name" Decoder.string
      <*> Decoder.field "url" Decoder.string

decodeMetadata :: Decoder Metadata
decodeMetadata =
  Metadata <$> Decoder.field "languages" (Decoder.list Language.fromJson)
           <*> Decoder.field "topics" (Decoder.list Decoder.string)

instance DBFormat Blog where
  encode = Text.pack . show . toJson
  decode = Utils.eitherToMaybe . (=<<) (Decoder.run decoder) . Json.parseJson . Text.unpack

toBlogItem :: Blog -> Json
toBlogItem blog = JsonObject
  [ ("identifier", JsonNumber $ fromIntegral $ identifier blog)
  , ("title", JsonString $ title blog)
  , ("date", Date.toJson $ date blog)
  , ("intro", JsonString $ intro blog)
  , ("languages", JsonArray $ map (JsonString . show) $ languages $ metadata blog)
  , ("topics", JsonArray $ map JsonString $ topics $ metadata blog)
  ]

readGyurblog :: String -> IO (Either String Blog)
readGyurblog path = handleFileContents . fmap Text.unpack <$> Utils.safeReadTextFile p
  where
    handleFileContents :: Maybe String -> Either String Blog
    handleFileContents maybeContents = Utils.maybeToEither "Failed to read file." maybeContents >>= parseGyurblog
    p = path ++ if suffix `List.isSuffixOf` path then "" else suffix
    suffix = ".gyurblog"

parseGyurblog :: String -> Either String Blog
parseGyurblog contents = do
  (s0, title) <- getTitle (lines contents)
  (s1, date) <- getDate s0
  (s2, langs) <- getLanguages s1
  (s3, tags) <- getTopics s2
  (s4, intro) <- getIntro s3
  return Blog
    { identifier = 0 -- Deal with this later (just pass it in from outside)
    , title = title
    , date = date
    , intro = intro
    , sections = []
    , references = []
    , metadata = Metadata
      { languages = langs
      , topics = tags
      }
    }

getTitle :: [String] -> Either String ([String], String)
getTitle s
  | any (List.isPrefixOf "title:") s =
    let (rest, title) = getPrefix "title:" "" s
    in if null title then Left "Title can't be blank!" else Right (rest, title)
  | otherwise = Utils.maybeToEither "Did not find title." $ (\(h, r) -> (r, Utils.trim h)) <$> Utils.safeDeHead s

getDate :: [String] -> Either String ([String], Date)
getDate s
  | any (List.isPrefixOf "date:") s =
    let (rest, dateStr) = getPrefix "date:" "" s
    in Utils.maybeToEither "Invalid Date!"  $ (rest,) <$> Date.parseDate dateStr
  | otherwise = do
      (rest, dateStr) <- Utils.maybeToEither "Did not find date." $ (\(h, r) -> (r, h)) <$> Utils.safeDeHead s
      Utils.maybeToEither "Invalid Date!"  $ (rest,) <$> Date.parseDate dateStr

getLanguages :: [String] -> Either String ([String], [Language])
getLanguages s
  | any (List.isPrefixOf "lang:") s =
    let (rest, langs) = getPrefix "lang:" "" s
    in Utils.maybeToEither "Invalid Language!"  $ (rest,) <$> Utils.safeRead ("[" ++ langs ++ "]")
  | otherwise = Right (s, [EN])

getTopics :: [String] -> Either String ([String], [String])
getTopics s =
  let (rest, tags) = getPrefix "tags:" "" s
  in Right (rest, words $ Utils.mapIf (==',') (const ' ') tags)

getIntro :: [String] -> Either String ([String], String)
getIntro = Right . getPrefix "(" ")"

getPrefix :: String -> String -> [String] -> ([String], String)
getPrefix prefix suffix s =
  let (relevant, rest) = List.partition (\a -> List.isPrefixOf prefix a && List.isSuffixOf suffix a) s
      dateStr = concatMap (Utils.trim . Utils.stripPrefix prefix . Utils.stripSuffix suffix) relevant
  in (rest, dateStr)
