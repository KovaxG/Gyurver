module Main where

import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
 
main :: IO ()
main = runServer (IP "localhost") (Port 8080) process

process :: Request -> IO Response
process _ = return $ mkResponse OK "<h1>Gyurver</h1><p>Hello, and welcome to my server :)</p>"