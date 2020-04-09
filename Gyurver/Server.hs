module Gyurver.Server (runServer, IP(..), Port(..)) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Network.Simple.TCP (serve, HostPreference(..), send, recv, Socket, SockAddr)

import Gyurver.Gyurror
import Gyurver.Request
import Gyurver.Response
import Utils

type RequestProcessor = Request -> IO Response
data IP = IP String
data Port = Port Int

runServer:: IP -> Port -> RequestProcessor -> IO ()
runServer (IP address) (Port port) processRequest =
  serve (Host address) (show port) (processConnection processRequest)

processConnection :: RequestProcessor -> (Socket, SockAddr) -> IO ()
processConnection processRequest (connectionSocket, remoteAddr) = do
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
          putStrLn "[Error] I got a connection, but did not receive any message!"
        FailedParse msg -> do
          sendResponse (return parseFailedResponse)
          putStrLn "[Error] Failed to parse!"

    sendResponse :: IO Response -> IO ()
    sendResponse responseIO = 
      responseIO >>= send connectionSocket . toByteString

parseFailedResponse :: Response
parseFailedResponse = mkResponse BadRequest "Failed to decode request!"

receiveFailedResponse :: Response
receiveFailedResponse = 
  mkResponse BadRequest "I got a connection but no message... Not sure if anyone will ever see this :D"
