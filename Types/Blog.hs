{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Blog where

import           Component.Database (DBFormat(..))
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.List as List
import           Data.List ((\\))
import           Data.Function ((&))
import qualified Data.Maybe as Maybe
import           Data.Monoid ((<>))
import           Types.Date (Date(..))
import qualified Types.Date as Date
import           Types.Language (Language(..))
import qualified Types.Language as Language
import qualified Text.Parsec as Parsec
import qualified Utils
import Language.Haskell.TH (pragAnnD)

type Topic = Text
type Title = Text

data Blog = Blog
  { identifier :: Int
  , title :: Title
  , date :: Date
  , intro :: Text
  , sections :: [Section]
  , references :: [Reference]
  , metadata :: Metadata
  } deriving (Show, Eq)

data Section = Paragraph Text deriving (Show, Eq)

wordCount :: Section -> Int
wordCount (Paragraph body) = length $ Text.words body

data Reference = Ref Int Text Text deriving (Show, Eq)

data Metadata = Metadata
  { languages :: [Language]
  , topics :: [Topic]
  } deriving (Show, Eq)

showSectionASCII :: Section -> Text
showSectionASCII (Paragraph body) = body <> "\n"

showReferenceASCII :: Reference -> Text
showReferenceASCII (Ref i name url) = "[" <> Text.pack (show i) <> "] " <> name <> " - " <> url

showASCII :: Blog -> Text
showASCII blog = Text.unlines $ mconcat
  [ [title blog]
  , ["------------"]
  , [Text.pack $ show $ date blog]
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
  [ ("languages", JsonArray $ map (JsonString . Language.toString) languages)
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
  encode = Json.toString . toJson
  decode = Utils.eitherToMaybe . (=<<) (Decoder.run decoder) . Json.parseJson

toBlogItem :: Blog -> Json
toBlogItem blog = JsonObject
  [ ("identifier", JsonNumber $ fromIntegral $ identifier blog)
  , ("title", JsonString $ title blog)
  , ("date", Date.toJson $ date blog)
  , ("intro", JsonString $ intro blog)
  , ("languages", JsonArray $ map (JsonString . Language.toString) $ languages $ metadata blog)
  , ("topics", JsonArray $ map JsonString $ topics $ metadata blog)
  , ("words", JsonNumber $ fromIntegral $ sum $ map wordCount $ sections blog)
  ]

readGyurblog :: Int -> Text -> IO (Either String Blog)
readGyurblog index path = handleFileContents <$> Utils.safeReadTextFile p
  where
    handleFileContents :: Maybe Text -> Either String Blog
    handleFileContents maybeContents = Utils.maybeToEither "Failed to read file." maybeContents >>= parseGyurblog index
    p = path <> if suffix `Text.isSuffixOf` path then "" else suffix
    suffix = ".gyurblog"

-- TODO maybe use a monad transformer here?
parseGyurblog :: Int -> Text -> Either String Blog
parseGyurblog index contents = do
  (s0, title) <- getTitle (filter (not . Text.null) $ map Utils.trim $ Text.lines contents)
  (s1, date) <- getDate s0
  (s2, langs) <- getLanguages s1
  (s3, tags) <- getTopics s2
  (s4, intro) <- getIntro s3
  (s5, refs) <- getReferences s4
  let secs = getSections s5
  _ <- checkRefs refs secs
  return Blog
    { identifier = index
    , title = title
    , date = date
    , intro = intro
    , sections = secs
    , references = refs
    , metadata = Metadata { languages = langs, topics = tags }
    }

getTitle :: [Text] -> Either String ([Text], Text)
getTitle s
  | any (Text.isPrefixOf "title:") s =
    let (rest, title) = getPrefix "title:" "" s
    in if Text.null title then Left "Title can't be blank!" else Right (rest, title)
  | otherwise = Utils.maybeToEither "Did not find title." $ (\(h, r) -> (r, Utils.trim h)) <$> Utils.safeDeHead s

getDate :: [Text] -> Either String ([Text], Date)
getDate s
  | any (Text.isPrefixOf "date:") s =
    let (rest, dateStr) = getPrefix "date:" "" s
    in Utils.maybeToEither "Invalid Date!"  $ (rest,) <$> Date.parseDate dateStr
  | otherwise = do
      (rest, dateStr) <- Utils.maybeToEither "Did not find date." $ (\(h, r) -> (r, h)) <$> Utils.safeDeHead s
      Utils.maybeToEither "Invalid Date!"  $ (rest,) <$> Date.parseDate dateStr

getLanguages :: [Text] -> Either String ([Text], [Language])
getLanguages s
  | any (Text.isPrefixOf "lang:") s =
    let (rest, langs) = getPrefix "lang:" "" s
    in Utils.maybeToEither "Invalid Language!"  $ (rest,) <$> Utils.safeRead ("[" <> langs <> "]")
  | otherwise = Right (s, [EN])

getTopics :: [Text] -> Either String ([Text], [Text])
getTopics s =
  let (rest, tags) = getPrefix "tags:" "" s
  in Right (rest, Text.words $ Text.replace "," " " tags)

getIntro :: [Text] -> Either String ([Text], Text)
getIntro = Right . getPrefix "(" ")"

getReferences :: [Text] -> Either String ([Text], [Reference])
getReferences s =
  let (relevant, rest) = List.partition isRef s
  in (rest,) <$> traverse parseRef relevant

isRef :: Text -> Bool
isRef = Text.isPrefixOf "[" . Utils.trim

parseRef :: Text -> Either String Reference
parseRef = Utils.mapLeft (const "Invalid Refeference.") . Parsec.parse rule "Parsing Reference"
  where
    rule = do
      Parsec.char '['
      index <- read <$> Parsec.many1 Parsec.digit
      Parsec.char ']'
      Parsec.spaces
      title <- Text.pack <$> Parsec.many (Parsec.noneOf "(")
      Parsec.char '('
      link <- Text.pack <$> Parsec.many (Parsec.noneOf ")")
      Parsec.char ')'
      return $ Ref index (Utils.trim title) link

getSections :: [Text] -> [Section]
getSections = map Paragraph

getPrefix :: Text -> Text -> [Text] -> ([Text], Text)
getPrefix prefix suffix s =
  let (relevant, rest) = List.partition (\a -> Text.isPrefixOf prefix a && Text.isSuffixOf suffix a) s
      dateStr = Text.concat $ map (Utils.trim . Utils.stripPrefix prefix . Utils.stripSuffix suffix) relevant
  in (rest, dateStr)

checkRefs :: [Reference] -> [Section] -> Either String ()
checkRefs refs secs =
  let textIndexes =
        secs
        & map (\(Paragraph s) -> s)
        & Text.concat
        & Text.filter (\c -> Text.any (==c) "[]0123456789")
        & Text.map (\c -> if Text.any (==c) "[]" then ' ' else c)
        & Text.words
        & filter (\w -> Text.head w == ' ')
        & Maybe.mapMaybe Utils.safeRead
      refIndexes = map (\(Ref i _ _) -> i) refs
      nonExistentRefs = textIndexes \\ refIndexes
      extraRefs = refIndexes \\ textIndexes
  in if not $ null nonExistentRefs
     then Left $ "Referencing non existent ref: " ++ show nonExistentRefs
     else if not $ null extraRefs
     then Left $ "Not referenced in the text: " ++ show extraRefs
     else Right ()

data Index = Index Int Text

withIndex :: Int -> Index -> Bool
withIndex i2 (Index i1 _) = i1 == i2

getFileName :: Index -> Text
getFileName (Index _ s) = s


toStringIndex (Index i s) = Text.pack (show i) <> " " <> s

instance DBFormat Index where
  encode = toStringIndex
  decode s =
    let (a, b) = Text.span Char.isDigit s
    in Index <$> Utils.safeRead a <*> return (Text.drop 1 b)
