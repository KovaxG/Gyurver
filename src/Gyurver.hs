module Gyurver (runServer, IP(..), Port(..)) where

import Data.ByteString.Char8 (pack, unpack)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Network.Simple.TCP (serve, HostPreference(..), send, recv, Socket, SockAddr)
import Text.Parsec (parse, string, (<|>), spaces, alphaNum, many, char, option, sepBy, noneOf, newline, anyChar)

-- TODO need to make a Response
type RequestProcessor = Request -> String
data IP = IP String
data Port = Port Int

runServer:: IP -> Port -> RequestProcessor -> IO ()
runServer (IP address) (Port port) processRequest =
  serve (Host address) (show port) (processConnection processRequest)

processConnection :: RequestProcessor -> (Socket, SockAddr) -> IO ()
processConnection processRequest (connectionSocket, remoteAddr) =
  recv connectionSocket 1024
  >>= maybe failedToRecieveMessage (sendResponse connectionSocket . unpack)
  where
    failedToRecieveMessage :: IO ()
    failedToRecieveMessage =
      putStrLn "[Error] I got a connection, but did not receive any message!"

    sendResponse :: Socket -> String -> IO ()
    sendResponse connectionSocket =
      send connectionSocket
      . pack
      . fromMaybe "omg"
      . fmap processRequest
      . parseRequest


data RequestType = Get | Post deriving (Show)
data Request = Request
  { requestType :: RequestType
  , path :: String
  , query :: Map String String
  , attributes :: Map String String
  , content :: String
  } deriving (Show)

parseRequest :: String -> Maybe Request
parseRequest = either (const Nothing) Just . parse request "URL" . removeCarries
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
    pathChars = alphaNum <|> char '/' <|> char '.'

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

($>) = flip (<$)