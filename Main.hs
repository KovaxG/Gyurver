{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (log)
import qualified Data.ByteString as BS

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
process Request{requestType, path} = do
  case (requestType, path) of
    (Get, "/") -> do
      info log $ "Requested landing page, sending " ++ landingPage
      contents <- readFile landingPage
      return $ makeResponse OK contents
    (Get, "/cv") -> do
      info log $ "Requested CV."
      contents <- BS.readFile cvPath
      return $ makeResponse OK contents
    (Get, "/favicon.ico") -> do
      info log $ "Requested favicon."
      contents <- BS.readFile faviconPath
      return $ makeResponse OK contents
    (Get, other) -> do
      info log $ "[GET " ++ other ++ "] No such thing, blaming the user."
      return $ makeResponse BadRequest badRequest
    _ ->
      return $ makeResponse BadRequest badRequest

landingPage = "Content/landing.html"
cvPath = "Content/pdfs/cv.pdf"
faviconPath = "Content/favicon.ico"
badRequest = "<h1>Bad Request</h1>"