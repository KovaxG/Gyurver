module Gyurver.Logger (Logger(..), info, error) where

import Prelude hiding (error)

data Logger = Console

info :: Logger -> String -> IO ()
info Console msg = putStrLn $ "[INFO] " ++ msg

error :: Logger -> String -> IO ()
error Console msg = putStrLn $ "[ERROR] " ++ msg
