{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (log)
import qualified Data.ByteString as BS

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
  putStrLn "Gyurver is starting."
  runServer log (IP "localhost") (Port 8080) process

process :: Request -> IO Response
process Request{requestType, path} = case (requestType, path) of
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
  (Post, path) -> do
    info log $ "[POST " ++ path ++ "] No such thing, blaming the user."
    badRequest

landingPagePath = "Content/landing.html"
cvPath = "Content/pdfs/cv.pdf"
faviconPath = "Content/favicon.ico"
articlesPath = "Content/articles.html"

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