{-# LANGUAGE OverloadedStrings #-}

module Gyurver.Server (run, IP(..), Port(..)) where

import Network.Simple.TCP (serve, HostPreference(..), send, recv, Socket, SockAddr)
import           Data.Text (Text)
import qualified Data.Text as Text

import Gyurver.Gyurror (Gyurror(..))
import Gyurver.Request (parseRequest, Request)
import Gyurver.Response (make, toByteString, Response, Status(BadRequest))
import Gyurver.Logger (Logger)
import qualified Gyurver.Logger as Log
import Utils (maybeToEither)

type RequestProcessor = Request -> IO Response
newtype IP = IP String deriving (Show, Eq)
newtype Port = Port Int deriving (Show, Eq)

run:: Logger -> IP -> Port -> RequestProcessor -> IO ()
run log (IP address) (Port port) processRequest =
  serve (Host address) (show port) (processConnection log processRequest)

processConnection :: Logger -> RequestProcessor -> (Socket, SockAddr) -> IO ()
processConnection log processRequest (connectionSocket, _) = do
  getMessage >>= either handleFailure (sendResponse . processRequest)
  where
    getMessage :: IO (Either Gyurror Request)
    getMessage = do
      raw <- maybeToEither FailedReceive <$> recv connectionSocket 1024
      return $ raw >>= parseRequest

    handleFailure :: Gyurror -> IO ()
    handleFailure error =
      case error of
        FailedReceive -> do
          sendResponse receiveFailedResponse
          Log.warn log "I got a connection, but did not receive any message!"
        FailedParse msg -> do
          sendResponse parseFailedResponse
          Log.error log $ "Failed to parse request with message: \"" <> msg <> "\""

    sendResponse :: IO Response -> IO ()
    sendResponse responseIO =
      responseIO >>= send connectionSocket . toByteString

parseFailedResponse :: IO Response
parseFailedResponse = make BadRequest ("Failed to decode request!" :: Text)

receiveFailedResponse :: IO Response
receiveFailedResponse =
  make BadRequest ("I got a connection but no message... Not sure if anyone will ever see this :D" :: Text)
