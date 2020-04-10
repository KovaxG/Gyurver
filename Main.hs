{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

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
      return $ mkResponse OK contents
    (Get, other) -> do
      info log $ "[GET " ++ other ++ "] No such thing, blaming the user."
      return $ mkResponse BadRequest badRequest
    _ ->
      return $ mkResponse BadRequest badRequest

landingPage = "Content/landing.html"
badRequest = "<h1>Bad Request</h1>"