{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (log)
import qualified Data.ByteString as BS

import Component.Database as DB

import Events.Cokkolo

import Gyurver.Html
import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
import Gyurver.Logger
import Utils

log :: Logger
log = Console

main :: IO ()
main = do
  putStrLn "Gyurver is starting..."
  tojasDB <- DB.getHandle "cokkolo2020"
  weirdRequestDB <- DB.getHandle "weird_requests"
  host <- maybe "localhost" id <$> safeReadFile "gyurver.settings"
  putStrLn $ "Ok, running on " ++ host
  runServer log (IP host) (Port 8080) (process tojasDB weirdRequestDB)

process :: DBHandle Tojas -> DBHandle Request -> Request -> IO Response
process tojasDB weirdRequestDB request@Request{requestType, path, content} = 
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
      sendFile articlesPath
    (Get, "/cokk/list") -> do
      info log $ "Requested cokkolesi lista."
      tojasok <- DB.everythingList tojasDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ tojasokToJson tojasok
    (Get, "/cokk/eredmeny") -> do
      info log $ "Requested results."
      sendFile resultsPath
    (Get, "/cokk") -> do
      info log $ "Requested add egg page."
      sendFile eggListPath
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
            badRequest
      | otherwise -> do
        info log $ "Adding [GET " ++ path ++ "] to weird request DB."
        DB.insert weirdRequestDB request
        badRequest
    (Post, path) -> do
      info log $ "Adding [POST " ++ path ++ "] to weird request DB."
      DB.insert weirdRequestDB request
      badRequest
    (Options, path) -> do
      info log $ "Adding [OPTIONS " ++ path ++ "] to weird request DB."
      DB.insert weirdRequestDB request
      badRequest

landingPagePath = "Content/landing.html"
cvPath = "Content/pdfs/cv.pdf"
faviconPath = "Content/favicon.ico"
articlesPath = "Content/articles.html"
eggListPath = "Content/egglist.html"
resultsPath = "Content/eredmenyek.html"
mainPath = "Content/main.html"

badRequest :: IO Response
badRequest =
  return
  $ makeResponse BadRequest
  $ Document [title [] [text "Gyurver"]] [h1 [] [text "Bad Request"]]

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
