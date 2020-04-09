module Main where

import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
 
main :: IO ()
main = runServer (IP "localhost") (Port 8080) process

process :: Request -> IO Response
process _ = do
  contents <- readFile "Content/index.html"
  return $ mkResponse OK contents