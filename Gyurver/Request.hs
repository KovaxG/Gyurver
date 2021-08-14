module Gyurver.Request (Request(..), RequestType(..), parseRequest) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.ByteString.Char8 (ByteString, unpack)
import           Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (parse, string, (<|>), spaces, alphaNum, many, char, option, sepBy, noneOf, newline, anyChar)

import Gyurver.Gyurror
import Utils

data RequestType = Get | Post | Options | Delete deriving (Read)

instance Show RequestType where
  show rt = case rt of
    Get -> "GET"
    Post -> "POST"
    Options -> "OPTIONS"
    Delete -> "DELETE"

data Request = Request
  { requestType :: RequestType
  , path :: Text
  , query :: Map Text Text
  , attributes :: Map Text Text
  , content :: Text
  } deriving (Show, Read)

parseRequest :: ByteString -> Either Gyurror Request
parseRequest =
  mapLeft (FailedParse . Text.pack . show)
  . parse request "Request"
  . removeCarries
  . unpack
  where
    removeCarries :: String -> String
    removeCarries = filter (/='\r')

    request = do
      requestType <- requestType
      spaces
      path <- path
      query <- query
      _ <- many (noneOf "\n")
      newline
      attributes <- attributes
      newline
      content <- Text.pack <$> many anyChar
      return Request {
        requestType = requestType,
        path = path,
        query = query,
        attributes = attributes,
        content = content
      }

    requestType = getRequest <|> postRequest <|> optionsRequest <|> deleteRequest
    getRequest = string "GET" $> Get
    postRequest = string "POST" $> Post
    optionsRequest = string "OPTIONS" $> Options
    deleteRequest = string "DELETE" $> Delete

    path = Text.pack <$> many pathChars
    pathChars = alphaNum <|> char '/' <|> char '.' <|> char '_'

    query = option Map.empty queryPairs
    queryPairs = do
      char '?'
      pairs <- sepBy queryPair (char '&')
      return $ Map.fromList pairs
    queryPair = do
      key <- Text.pack <$> many alphaNum
      char '='
      value <- Text.pack <$> many alphaNum
      return (key, value)

    attributes = Map.fromList <$> many attribute
    attribute = do
      key <- Text.pack <$> many keyChar
      string ": "
      value <- Text.pack <$> many valueChar
      newline
      return (key, value)
    keyChar = alphaNum <|> char '-'
    valueChar = noneOf "\n"
