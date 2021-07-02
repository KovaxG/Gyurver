module Types.Blog where

import           Component.Database (DBFormat(..))
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Data.Text as Text
import           Types.Date (Date(..))
import qualified Types.Date as Date
import qualified Utils

type Language = String
type Topic = String
type Title = String

data Blog = Blog
  { title :: Title
  , date :: Date
  , sections :: [Section]
  , references :: [Reference]
  , metadata :: Metadata
  } deriving (Show)

data Section = Paragraph String deriving (Show)

data Reference = Ref Int String String deriving (Show)

data Metadata = Metadata [Language] [Topic] deriving (Show)

showSectionASCII :: Section -> String
showSectionASCII (Paragraph body) = body ++ "\n"

showReferenceASCII :: Reference -> String
showReferenceASCII (Ref i name url) = "[" ++ show i ++ "] " ++ name ++ " - " ++ url

showASCII :: Blog -> String
showASCII blog = unlines $ mconcat
  [ [title blog]
  , ["------------"]
  , [""]
  , map showSectionASCII (sections blog)
  , [""]
  , map showReferenceASCII (references blog)
  ]

testBlog :: Blog
testBlog = Blog
  { title = "Test Blog"
  , date = Date 2021 7 2
  , sections =
      [ Paragraph "Hey, this is the first test blog. It is not intended to be shared, it is only intended for testing. Here is how I will refer to stuff: I like youtube [1]"
      , Paragraph "This is the second paragraph. I think I will not add newlines in paragraphs, because that way it will be much simpler for me to work with the stuff."
      ]
  , references =
      [ Ref 1 "Youtube" "www.youtube.com"
      , Ref 2 "Google" "www.google.com"
      ]
  , metadata = Metadata ["English"] ["test"]
  }

toJson :: Blog -> Json
toJson blog = JsonObject
  [ ("title", JsonString $ title blog)
  , ("date", Date.toJson $ date blog)
  , ("sections", JsonArray $ map sectionToJson $ sections blog)
  , ("references", JsonArray $ map refToJson $ references blog)
  , ("metadata", metadataToJson $ metadata blog)
  ]

sectionToJson :: Section -> Json
sectionToJson (Paragraph body) =
  JsonObject [("section", JsonString "paragraph"), ("content", JsonString body)]

refToJson :: Reference -> Json
refToJson (Ref index name url) =
  JsonObject [("index", JsonNumber $ fromIntegral index), ("name", JsonString name), ("url", JsonString url)]


metadataToJson :: Metadata -> Json
metadataToJson (Metadata languages topics) =
  JsonObject [("languages", JsonArray $ map JsonString languages), ("topics", JsonArray $ map JsonString topics)]

decoder :: Decoder Blog
decoder =
  Blog <$> Decoder.field "title" Decoder.string
       <*> Decoder.field "date" Date.decoder
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
  Metadata <$> Decoder.field "languages" (Decoder.list Decoder.string)
           <*> Decoder.field "topics" (Decoder.list Decoder.string)

instance DBFormat Blog where
  encode = Text.pack . show . toJson
  decode = Utils.eitherToMaybe . (=<<) (Decoder.run decoder) . Json.parseJson . Text.unpack
