module Main where

import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
 
main :: IO ()
main = do
  putStrLn "Gyurver is starting." 
  runServer (IP "localhost") (Port 8080) process

process :: Request -> IO Response
process _ = do
  putStrLn "Got some request, whatever. Imma just send Contents/index.html"
  contents <- readFile "Content/index.html"
  return $ mkResponse OK contents
