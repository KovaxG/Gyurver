{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (log)
import qualified Data.ByteString as BS

import Component.Decoder (Decoder(..))
import Component.Database as DB
import Component.Json as Json

import Data.Function

import Events.Cokkolo

import Gyurver.Html
import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
import Gyurver.Logger as Logger
import Gyurver.Logger

import Types.Video (Video)
import qualified Types.Video as Video
import Types.VideoAddRequest
import Types.Settings as Settings

import Utils

log :: Logger
log = Console

main :: IO ()
main = do
  putStrLn "Gyurver is starting..."
  tojasDB <- DB.getHandle "cokkolo2020"
  weirdRequestDB <- DB.getHandle "weird_requests"
  vidsDB <- DB.getHandle "vids"
  settings <- readSettings log
  runServer log
            (IP $ hostAddress settings)
            (Port 8080)
            (process tojasDB weirdRequestDB vidsDB settings)

readSettings :: Logger -> IO Settings
readSettings log = do
  contentsMaybe <- safeReadFile "gyurver.settings"
  maybe fileNotFound fileFound contentsMaybe
  where
    fileNotFound :: IO Settings
    fileNotFound = do
      Logger.error log "Could not read settings file, using default settings."
      return defaultSettings

    fileFound :: String -> IO Settings
    fileFound contents = do
      either settingsParseFailed settingsLoaded (Settings.parse contents)

    settingsParseFailed :: String -> IO Settings
    settingsParseFailed msg = do
      Logger.error log "Found settings file, but failed to parse it, using default settings."
      return defaultSettings

    settingsLoaded :: Settings -> IO Settings
    settingsLoaded settings = do
      info log $ "Loaded settings, ip is " ++ show (hostAddress settings)
      return settings

process :: DBHandle Tojas
        -> DBHandle Request
        -> DBHandle Video
        -> Settings
        -> Request
        -> IO Response
process tojasDB
        weirdRequestDB
        vidsDB
        settings
        request@Request{requestType, path, content} =
  case (requestType, path) of
    (Get, "/") -> do
      info log $ "Requested landing page, sending " ++ mainPath
      sendFile mainPath
    (Get, "/cv") -> do
      info log $ "Requested CV."
      sendFile cvPath
    (Get, "/favicon.ico") -> do
      info log $ "Requested favicon."
      sendFile faviconPath
    (Get, "/articles") -> do
      info log $ "Requested articles page."
      sendFile mainPath
    (Get, "/cokk/list") -> do
      info log $ "[API] Requested cokkolesi lista."
      tojasok <- DB.everythingList tojasDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ tojasokToJson tojasok
    (Get, "/vids") -> do
      info log $ "Requested video list."
      sendFile mainPath
    (Get, "/api/vids") -> do
      info log $ "[API] Requested video list."
      videos <- DB.everythingList vidsDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ Video.videosToJson videos
    (Get, "/cokk/eredmeny") -> do
      info log $ "Requested results."
      sendFile mainPath
    (Get, "/cokk") -> do
      info log $ "Requested add egg page."
      sendFile mainPath
    (Get, "/vids/add") -> do
      info log $ "Requested video add page."
      sendFile mainPath
    (Get, path)
      | isResourceReq path -> do
        info log $ "Requesting resource [" ++ path ++ "]."
        case resourceType path of
          Just ft -> do
            let filePath = "Content/" ++ show ft ++ "s/" ++ fileName path ++ "." ++ show ft
            info log $ "Sending " ++ filePath ++"... Let's hope it exists..."
            sendFile filePath
          Nothing -> do
            info log $ "No such resource."
            return badRequest
      | otherwise -> do
        info log $ "Adding [GET " ++ path ++ "] to weird request DB."
        DB.insert weirdRequestDB request
        return badRequest

    (Post, "/api/vids") -> do
      info log $ "[API] Adding new video to list."
      let request = (run videoRequestDecoder =<< Json.parseJson content) :: Either String VideoAddRequest
      either
        (\errorMsg -> return $ makeResponse BadRequest errorMsg)
        (\request ->
          case videoRequestToVideo settings request of
            Just video -> do
              insert vidsDB video
              return $ makeResponse OK "Success"
            Nothing -> do
              info log "Bad Password"
              return $ makeResponse Unauthorized "Bad Password"
        )
        request
    (Post, path) -> do
      info log $ "Adding [POST " ++ path ++ "] to weird request DB."
      DB.insert weirdRequestDB request
      return badRequest

    (Options, "/api/vids") -> do
      info log $ "Someone asked if you can post to /api/vids/add, sure."
      return allowHeaders
    (Options, path) -> do
      info log $ "Adding [OPTIONS " ++ path ++ "] to weird request DB."
      DB.insert weirdRequestDB request
      return badRequest

cvPath = "Content/pdfs/cv.pdf"
faviconPath = "Content/favicon.ico"
mainPath = "Content/main.html"

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
sendFile path = do
  readFile path >>= return . makeResponse OK

isResourceReq :: String -> Bool
isResourceReq = startsWith "/res/"

data FileType = PDF

instance Show FileType where
  show ft = case ft of
    PDF -> "pdf"

parseFileType :: String -> Maybe FileType
parseFileType s = case s of
  "pdf" -> Just PDF
  _ -> Nothing

resourceType :: String -> Maybe FileType
resourceType =
  parseFileType
  . reverse
  . takeWhile (/= '.')
  . reverse

fileName :: String -> String
fileName =
  drop 5
  . reverse
  . tail
  . dropWhile (/= '.')
  . reverse
