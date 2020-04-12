{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (log)
import qualified Data.ByteString as BS

import Component.Database

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
  db <- newDB "cokkolo2020"
  host <- maybe "localhost" id <$> safeReadFile "gyurver.settings"
  putStrLn $ "Ok, running on " ++ host
  runServer log (IP host) (Port 8080) (process db)

process :: DB Tojas -> Request -> IO Response
process db Request{requestType, path, content} = case (requestType, path) of
  (Get, "/") -> do
    info log $ "Requested landing page, sending " ++ landingPagePath
    sendFile landingPagePath
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
    tojasok <- readDB db
    return
      $ addHeaders [("Content-Type", "application/json")]
      $ makeResponse OK
      $ tojasokToJson tojasok
  (Get, "/cokk/add") -> do
    info log $ "Requested add egg page."
    sendFile addEggPath
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
      info log $ "[GET " ++ path ++ "] No such thing, blaming the user."
      badRequest
  (Post, "/cokk/add") -> do
    info log $ "Adding new tojas " ++ content
    let tojas = read content :: Tojas
    appendDB db tojas
    return $ makeResponse OK "Tegyuk fel"
  (Post, path) -> do
    info log $ "[POST " ++ path ++ "] No such thing, blaming the user."
    badRequest
  (Options, "/cokk/add") -> do
    info log $ "People are asking if you can add eggs to cokk. The answer is yes."
    return
      $ addHeaders
        [ ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Headers", "*")
        , ("Access-Control-Allow-Methods", "OPTIONS, POST")
        ]
      $ makeResponse OK ""
  (Options, path) -> do
    info log $ "[OPTIONS " ++ path ++ "] No such thing, blaming the user."
    badRequest

landingPagePath = "Content/landing.html"
cvPath = "Content/pdfs/cv.pdf"
faviconPath = "Content/favicon.ico"
articlesPath = "Content/articles.html"
addEggPath = "Content/addegg.html"
eggListPath = "Content/egglist.html"

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