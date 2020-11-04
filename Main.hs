{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (log)

import Component.Decoder (Decoder(..))
import Component.Database as DB
import Component.Json as Json

import Data.Function ((&))

import Events.Cokkolo

import Gyurver.Html
import Gyurver.Request
    ( Request(Request, content, path, requestType),
      RequestType(Options, Post, Get) )
import Gyurver.Response
import Gyurver.Server
import           Gyurver.Logger (Logger(..))
import qualified Gyurver.Logger as Logger
import Types.Video (Video)
import qualified Types.Video as Video
import Types.VideoAddRequest
import Types.Settings as Settings
import Endpoints
import Utils

log :: Logger
log = File

main :: IO ()
main = do
  Logger.info log "Gyurver is starting..."

  tojasDB <- DB.getHandle "cokkolo2020"
  vidsDB <- DB.getHandle "vids"

  settings <- readSettings log
  Logger.info log (show settings)
  runServer log
            (settings & hostAddress)
            (settings & port)
            (process tojasDB vidsDB settings)

readSettings :: Logger -> IO Settings
readSettings log =
  safeReadTextFile "gyurver.settings" >>= maybe fileNotFound fileFound
  where
    fileNotFound :: IO Settings
    fileNotFound = do
      Logger.warn log "Could not read settings file, using default settings."
      return defaultSettings

    fileFound :: String -> IO Settings
    fileFound contents = contents & Settings.parse & either settingsParseFailed settingsLoaded

    settingsParseFailed :: String -> IO Settings
    settingsParseFailed msg = do
      let message = "Found settings file, but failed to parse it (" ++ msg ++ "), using default settings."
      Logger.error log message
      return defaultSettings

    settingsLoaded :: Settings -> IO Settings
    settingsLoaded settings = do
      Logger.info log $ "Loaded settings, ip is " ++ show (hostAddress settings)
      return settings

process :: DBHandle Tojas
        -> DBHandle Video
        -> Settings
        -> Request
        -> IO Response
process tojasDB
        vidsDB
        settings
        Request{requestType, path, content} =
  case parseEndpoint $ unwords [show requestType, path] of
    GetLandingPage -> do
      Logger.info log $ "Requested landing page, sending " ++ mainPath
      sendFile mainPath

    GetCV -> do
      Logger.info log $ "Requested CV."
      sendFile cvPath

    GetFavicon -> do
      Logger.info log $ "Requested favicon."
      sendFile faviconPath

    GetArticlesPage -> do
      Logger.info log $ "Requested articles page."
      sendFile mainPath

    GetCokkJSON -> do
      Logger.info log $ "[API] Requested cokkolesi lista."
      tojasok <- DB.everythingList tojasDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ tojasokToJson tojasok

    GetVideosPage -> do
      Logger.info log $ "Requested video list."
      sendFile mainPath

    GetVideosJSON -> do
      Logger.info log $ "[API] Requested video list."
      videos <- DB.everythingList vidsDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ Video.videosToJson videos

    GetCokkResultsPage -> do
      Logger.info log $ "Requested results."
      sendFile mainPath

    GetCokkPage -> do
      Logger.info log $ "Requested add egg page."
      sendFile mainPath

    GetVideosAddPage -> do
      Logger.info log $ "Requested video add page."
      sendFile mainPath

    GetResource resource -> do
      Logger.info log $ "Requesting resource [" ++ resource ++ "]."
      case resourceType resource of
        Just ft -> do
          let filePath = contentPath </> (show ft ++ "s") </> fileName path ++ "." ++ show ft
          Logger.info log $ "Sending " ++ filePath ++"... Let's hope it exists..."
          sendFile filePath
        Nothing -> do
          Logger.warn log $ "No such resource: " ++ path
          return badRequest

    PostVideo -> do
      Logger.info log $ "[API] Adding new video to list."
      let request = Json.parseJson content >>= run videoRequestDecoder
      either
        (\errorMsg -> return $ makeResponse BadRequest errorMsg)
        (\request ->
          case videoRequestToVideo settings request of
            Right video -> do
              insert vidsDB video
              return success
            Left error -> do
              Logger.info log error
              return $ makeResponse Unauthorized error
        )
        request

    OptionsVideo -> do
      Logger.info log $ "Someone asked if you can post to /api/vids/add, sure."
      return allowHeaders

    Other req -> do
      Logger.warn log $ "Weird request: " ++ req
      return badRequest

contentPath :: String
contentPath = "Content"

cvPath :: String
cvPath = contentPath </> "pdfs" </> "cv.pdf"

faviconPath :: String
faviconPath = contentPath </> "favicon.ico"

mainPath :: String
mainPath = contentPath </> "index.html"

allowHeaders :: Response
allowHeaders =
  "Wanna try posting stuff? Go ahead."
  & makeResponse OK
  & addHeaders
    [ ("Access-Control-Allow-Headers", "OPTIONS, POST")
    , ("Access-Control-Allow-Origin",  "*") -- Added to allow requests from localhost
    ]

badRequest :: Response
badRequest =
  makeResponse BadRequest
  $ Document
    [title [] [text "Gyurver"]]
    [h1 [] [text "Your request was bad, and you should feel bad. Nah, just messing with you, have a nice day, but your requests still suck tho."]]

sendFile :: String -> IO Response
sendFile path =
  safeReadBinaryFile path
  & fmap (maybe (makeResponse InternalServerError "Could not read file!") (makeResponse OK))

data FileType = PDF

instance Show FileType where
  show ft = case ft of
    PDF -> "pdf"

parseFileType :: String -> Maybe FileType
parseFileType s = case s of
  "pdf" -> Just PDF
  _ -> Nothing

resourceType :: String -> Maybe FileType
resourceType = parseFileType . reverse . takeWhile (/= '.') . reverse

fileName :: String -> String
fileName = drop 5 . reverse . tail . dropWhile (/= '.') . reverse

(</>) :: String -> String -> String
a </> b = a ++ "/" ++ b
