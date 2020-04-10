{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (log)
import qualified Data.ByteString as BS

import Gyurver.Html
import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
import Gyurver.Logger

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
  (Get, other) -> do
    info log $ "[GET " ++ other ++ "] No such thing, blaming the user."
    badRequest
  (Post, other) -> do
    info log $ "[POST " ++ other ++ "] No such thing, blaming the user."
    badRequest

landingPagePath = "Content/landing.html"
cvPath = "Content/pdfs/cv.pdf"
faviconPath = "Content/favicon.ico"

badRequest :: IO Response
badRequest =
  return
  $ makeResponse BadRequest
  $ Document [title [] [text "Gyurver"]] [h1 [] [text "Bad Request"]]

sendFile :: String -> IO Response
sendFile path = do
  readFile path >>= return . makeResponse OK