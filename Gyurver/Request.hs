module Gyurver.Request (Request(..), RequestType(..), parseRequest) where

import Data.ByteString.Char8 (ByteString, unpack)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (parse, string, (<|>), spaces, alphaNum, many, char, option, sepBy, noneOf, newline, anyChar)

import Gyurver.Gyurror
import Utils

data RequestType = Get | Post deriving (Show)
data Request = Request
  { requestType :: RequestType
  , path :: String
  , query :: Map String String
  , attributes :: Map String String
  , content :: String
  } deriving (Show)

parseRequest :: ByteString -> Either Gyurror Request
parseRequest =
  mapLeft (FailedParse . show)
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
      content <- many anyChar
      return Request {
        requestType = requestType,
        path = path,
        query = query,
        attributes = attributes,
        content = content
      }

    requestType = getRequest <|> postRequest
    getRequest = string "GET" $> Get
    postRequest = string "POST" $> Post

    path = many pathChars
    pathChars = alphaNum <|> char '/' <|> char '.' <|> char '_'

    query = option Map.empty queryPairs
    queryPairs = do
      char '?'
      pairs <- sepBy queryPair (char '&')
      return $ Map.fromList pairs
    queryPair = do
      key <- many alphaNum
      char '='
      value <- many alphaNum
      return (key, value)

    attributes = Map.fromList <$> many attribute
    attribute = do
      key <- many keyChar
      string ": "
      value <- many valueChar
      newline
      return (key, value)
    keyChar = alphaNum <|> char '-'
    valueChar = noneOf "\n"