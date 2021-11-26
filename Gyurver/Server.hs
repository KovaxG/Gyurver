{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Gyurver.Server (run, IP(..), Port(..)) where

import qualified Control.Exception as Exception
import           Network.Simple.TCP (serve, HostPreference(..), send, recv, Socket, SockAddr)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))

import           Gyurver.Gyurror (Gyurror(..))
import           Gyurver.Request (parseRequest, Request(..))
import           Gyurver.Response (make, toByteString, Response, Status(BadRequest))
import           Gyurver.Logger (Logger)
import qualified Gyurver.Logger as Log
import           Component.Database (DBHandle)
import qualified Component.Database as DB
import           Utils (maybeToEither)
import           Control.Exception (SomeException(SomeException))
import qualified Control.Concurrent as Concurrent

import           Types.PageHit (PageHit)
import qualified Types.PageHit as PageHit
import qualified Types.DateTime as DateTime

import qualified System.CPUTime as System

type RequestProcessor = Request -> IO Response
newtype IP = IP String deriving (Show, Eq)
newtype Port = Port Int deriving (Show, Eq)

run:: Logger -> DBHandle PageHit -> IP -> Port -> RequestProcessor -> IO ()
run log pageHitDB (IP address) (Port port) processRequest =
  Exception.catch
    (serve (Host address) (show port) (processConnection log pageHitDB processRequest))
    handleException
  where
    handleException :: SomeException -> IO ()
    handleException e = do
      Log.warn log $ "Failed to start server with error: " <> Text.pack (show e) <> ", restarting in " <> Text.pack (show waitSeconds) <> " seconds..."
      Concurrent.threadDelay (secondsToMicro waitSeconds)
      run log pageHitDB (IP address) (Port port) processRequest

    secondsToMicro s = s * 1000000

    waitSeconds = 30

processConnection :: Logger -> DBHandle PageHit -> RequestProcessor -> (Socket, SockAddr) -> IO ()
processConnection log pageHitDB processRequest (connectionSocket, _) = do
  t0 <- System.getCPUTime
  request <- getMessage
  either handleFailure
    (\r -> do
      sendResponse $ processRequest r
      t1 <- System.getCPUTime
      let millis = fromIntegral (div (t1 - t0) 1000000) / 1000
      savePageHit pageHitDB r millis
    )
    request
  where
    getMessage :: IO (Either Gyurror Request)
    getMessage = do
      raw <- maybeToEither FailedReceive <$> recv connectionSocket 2048
      return $ raw >>= parseRequest

    handleFailure :: Gyurror -> IO ()
    handleFailure error =
      case error of
        FailedReceive -> do
          sendResponse receiveFailedResponse
          -- I think this happens because the browser keeps opening connections and doesn't send any message
          -- I should Either try to stop the browser or quickly close the connection, or both, or neither somehow
          --Log.warn log "I got a connection, but did not receive any message!"
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

savePageHit :: DBHandle PageHit -> Request -> Double -> IO ()
savePageHit db Request{requestType, path} millis = do
  now <- DateTime.getCurrentDateTime
  let pageHit = PageHit.make now (Text.pack $ show requestType) path millis
  DB.insert db pageHit
