module Gyurver.Request (Request(..), RequestType(..), parseRequest) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.ByteString.Char8 (ByteString, unpack)
import           Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (parse, string, (<|>), spaces, alphaNum, many, many1, char, option, sepBy, noneOf, newline, anyChar, parserFail, parserReturn)

import Gyurver.Gyurror
import Utils

data RequestType = GET | POST | OPTIONS | DELETE | PUT deriving (Show, Read)

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

    requestType = do
      requestTypeString <- Text.pack <$> many1 alphaNum
      maybe (parserFail "Could not decode request type.") parserReturn $ safeRead requestTypeString

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
