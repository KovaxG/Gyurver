module Gyurver.Logger (Logger(..), info, error) where

import Prelude hiding (error)
import Data.Time

data Logger = Console

data LogMode = Info | Error

instance Show LogMode where
  show mode = case mode of 
    Info -> "INFO"
    Error -> "Error"

info :: Logger -> String -> IO ()
info Console = genericLog Console Info
  
error :: Logger -> String -> IO ()
error Console = genericLog Console Error
  
genericLog :: Logger -> LogMode -> String -> IO ()
genericLog logger logMode message = do
  time <- getZonedTime
  putStrLn $ sbrackets (show logMode ++ " " ++ show time) ++ " " ++ message

sbrackets :: String -> String
sbrackets s = "[" ++ s ++ "]"
