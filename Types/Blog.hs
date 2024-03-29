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
import qualified Data.Bifunctor as Bifunctor
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
  , footnotes :: [Footnote]
  , references :: [Reference]
  , metadata :: Metadata
  } deriving (Show, Eq)

data Section = Paragraph Text deriving (Show, Eq)

wordCount :: Section -> Int
wordCount (Paragraph body) = length $ Text.words body

data Reference = Ref Int Text Text deriving (Show, Eq)

data Footnote = Footnote Text Text deriving (Show, Eq)

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
      , Paragraph "Now we also support footnotes(*)."
      ]
  , footnotes =
      [ Footnote "*" "This is a footnote"
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
  , ("footnotes", JsonArray $ map footToJson $ footnotes blog)
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

footToJson :: Footnote -> Json
footToJson (Footnote symbol text) = JsonObject
  [ ("symbol", JsonString symbol)
  , ("text", JsonString text)
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
       <*> Decoder.field "footnotes" (Decoder.list decodeFootnote)
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

decodeFootnote :: Decoder Footnote
decodeFootnote =
  Footnote <$> Decoder.field "mark" Decoder.string
           <*> Decoder.field "note" Decoder.string


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

readGyurblog :: Int -> Text -> IO (Either Text Blog)
readGyurblog index path = handleFileContents <$> Utils.safeReadTextFile p
  where
    handleFileContents :: Maybe Text -> Either Text Blog
    handleFileContents maybeContents = Utils.maybeToEither "Failed to read file." maybeContents >>= parseGyurblog index
    p = path <> if suffix `Text.isSuffixOf` path then "" else suffix
    suffix = ".gyurblog"

-- TODO maybe use a monad transformer here?
-- might break Raspberry Pi compatibility :[]
parseGyurblog :: Int -> Text -> Either Text Blog
parseGyurblog index contents = do
  (s0, title) <- getTitle (filter (not . Text.null) $ map Utils.trim $ Text.lines contents)
  (s1, date) <- getDate s0
  (s2, langs) <- getLanguages s1
  (s3, tags) <- getTopics s2
  (s4, intro) <- getIntro s3
  (s5, refs) <- getReferences s4
  let secs = getSections s5
  let (secs2, footnotes) = getFootnotes secs
  _ <- checkRefs refs secs
  return Blog
    { identifier = index
    , title = title
    , date = date
    , intro = intro
    , sections = secs2
    , footnotes = footnotes
    , references = refs
    , metadata = Metadata { languages = langs, topics = tags }
    }

getTitle :: [Text] -> Either Text ([Text], Text)
getTitle s
  | any (Text.isPrefixOf "title:") s =
    let (rest, title) = getPrefix "title:" "" s
    in if Text.null title then Left "Title can't be blank!" else Right (rest, title)
  | otherwise = Utils.maybeToEither "Did not find title." $ (\(h, r) -> (r, Utils.trim h)) <$> Utils.safeDeHead s

getDate :: [Text] -> Either Text ([Text], Date)
getDate s
  | any (Text.isPrefixOf "date:") s =
    let (rest, dateStr) = getPrefix "date:" "" s
    in Utils.maybeToEither "Invalid Date!"  $ (rest,) <$> Date.parseDate dateStr
  | otherwise = do
      (rest, dateStr) <- Utils.maybeToEither "Did not find date." $ (\(h, r) -> (r, h)) <$> Utils.safeDeHead s
      Utils.maybeToEither "Invalid Date!"  $ (rest,) <$> Date.parseDate dateStr

getLanguages :: [Text] -> Either Text ([Text], [Language])
getLanguages s
  | any (Text.isPrefixOf "lang:") s =
    let (rest, langs) = getPrefix "lang:" "" s
    in Utils.maybeToEither "Invalid Language!"  $ (rest,) <$> Utils.safeRead ("[" <> langs <> "]")
  | otherwise = Right (s, [EN])

getTopics :: [Text] -> Either Text ([Text], [Text])
getTopics s =
  let (rest, tags) = getPrefix "tags:" "" s
  in Right (rest, Text.words $ Text.replace "," " " tags)

getIntro :: [Text] -> Either Text ([Text], Text)
getIntro = Right . getPrefix "(" ")"

getReferences :: [Text] -> Either Text ([Text], [Reference])
getReferences s =
  let (relevant, rest) = List.partition isRef s
  in (rest,) <$> traverse parseRef relevant

isRef :: Text -> Bool
isRef = Text.isPrefixOf "[" . Utils.trim

parseRef :: Text -> Either Text Reference
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

getFootnotes :: [Section] -> ([Section], [Footnote])
getFootnotes secs =
  if wellFormed texts
  then  (map Paragraph $ Text.lines $ maskFootnotes semantic, extractFootnotes semantic)
  else (secs, [])
  where
    semantic = parseSemanticText $ Text.unlines texts
    texts = map sectionText secs

sectionText :: Section -> Text
sectionText (Paragraph txt) = txt

wellFormed :: [Text] -> Bool
wellFormed = (==0)
           . sum
           . fmap toNr
           . Text.unpack
           . Text.filter (`elem` ['(', ')'])
           . Text.concat
  where
    toNr :: Char -> Int
    toNr '(' = 1
    toNr _ = -1

getPrefix :: Text -> Text -> [Text] -> ([Text], Text)
getPrefix prefix suffix s =
  let (relevant, rest) = List.partition (\a -> Text.isPrefixOf prefix a && Text.isSuffixOf suffix a) s
      dateStr = Text.concat $ map (Utils.trim . Utils.stripPrefix prefix . Utils.stripSuffix suffix) relevant
  in (rest, dateStr)

checkRefs :: [Reference] -> [Section] -> Either Text ()
checkRefs refs secs =
  let textIndexes =
        secs
        & map (\(Paragraph s) -> s)
        & Text.concat
        & Text.filter (\c -> Text.any (==c) "[]0123456789")
        & Text.concatMap (\c -> if c == '[' then " [" else if c == ']' then "] " else Text.singleton c)
        & Text.words
        & filter (\w -> Text.isPrefixOf (Text.singleton '[') w && Text.isSuffixOf (Text.singleton ']') w)
        & Maybe.mapMaybe (Utils.safeRead . Text.init . Text.tail)
      refIndexes = map (\(Ref i _ _) -> i) refs
      nonExistentRefs = textIndexes \\ refIndexes
      extraRefs = refIndexes \\ textIndexes
  in if not $ null nonExistentRefs
     then Left $ "Referencing non existent ref: " <> Text.pack (show nonExistentRefs)
     else if not $ null extraRefs
     then Left $ "Not referenced in the text: " <> Text.pack (show extraRefs)
     else Right ()

data Index = Index Int Text

withIndex :: Int -> Index -> Bool
withIndex i2 (Index i1 _) = i1 == i2

getFileName :: Index -> Text
getFileName (Index _ s) = s

toStringIndex :: Index -> Text
toStringIndex (Index i s) = Text.pack (show i) <> " " <> s

instance DBFormat Index where
  encode = toStringIndex
  decode s =
    let (a, b) = Text.span Char.isDigit s
    in Index <$> Utils.safeRead a <*> return (Text.drop 1 b)

data SemanticText = Out Text | In Text deriving (Show)

isIn :: SemanticText -> Bool
isIn (In _) = True
isIn _ = False

getSemanticText :: SemanticText -> Text
getSemanticText (In t) = t
getSemanticText (Out t) = t

-- TODO if the brackest are ill formed so is the semantic text
parseSemanticText :: Text -> [SemanticText]
parseSemanticText =
  filter ((/="") . getSemanticText)
  . (\(sts, t) -> sts ++ [Out t])
  . Text.foldl rule ([], Text.empty)
  where
    rule :: ([SemanticText], Text) -> Char -> ([SemanticText], Text)
    rule (sts, t) '(' = (sts ++ [Out t], Text.empty)
    rule (sts, t) ')' = (sts ++ [In t], Text.empty)
    rule (sts, t) c = (sts, Text.snoc t c)

extractFootnotes :: [SemanticText] -> [Footnote]
extractFootnotes =
  zipWith (Footnote . Utils.foot) [0 ..]
  . map (Text.strip . Text.drop (Text.length "footnote"))
  . filter (Text.isPrefixOf "footnote")
  . map getSemanticText
  . filter isIn

maskFootnotes :: [SemanticText] -> Text
maskFootnotes = fst . foldl rule (Text.empty, 0)
  where
    rule :: (Text, Int) -> SemanticText -> (Text, Int)
    rule (txt, n) (Out t) = (Text.append txt t, n)
    rule (txt, n) (In t)
      | Text.isPrefixOf "footnote" t = (Text.append txt (Utils.foot n), n + 1)
      | otherwise = (Text.concat [txt, "(", t, ")"], n)
