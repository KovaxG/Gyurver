module Gyurver.Server (runServer, IP(..), Port(..)) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Network.Simple.TCP (serve, HostPreference(..), send, recv, Socket, SockAddr)

import Gyurver.Gyurror
import Gyurver.Request
import Gyurver.Response
import Gyurver.Logger (Logger)
import qualified Gyurver.Logger as Log
import Utils

type RequestProcessor = Request -> IO Response
data IP = IP String
data Port = Port Int

runServer:: Logger -> IP -> Port -> RequestProcessor -> IO ()
runServer log (IP address) (Port port) processRequest =
  serve (Host address) (show port) (processConnection log processRequest)

processConnection :: Logger -> RequestProcessor -> (Socket, SockAddr) -> IO ()
processConnection log processRequest (connectionSocket, remoteAddr) = do
  getMessage >>= either handleFailure (sendResponse . processRequest)
  where
    getMessage :: IO (Either Gyurror Request)
    getMessage = do
      raw <- toRight FailedReceive <$> recv connectionSocket 1024
      return $ raw >>= parseRequest

    handleFailure :: Gyurror -> IO ()
    handleFailure error =
      case error of
        FailedReceive -> do
          sendResponse (return receiveFailedResponse)
          Log.error log "I got a connection, but did not receive any message!"
        FailedParse msg -> do
          sendResponse (return parseFailedResponse)
          Log.error log "Failed to parse!"

    sendResponse :: IO Response -> IO ()
    sendResponse responseIO =
      responseIO >>= send connectionSocket . toByteString

parseFailedResponse :: Response
parseFailedResponse = makeResponse BadRequest "Failed to decode request!"

receiveFailedResponse :: Response
receiveFailedResponse =
  makeResponse BadRequest "I got a connection but no message... Not sure if anyone will ever see this :D"
