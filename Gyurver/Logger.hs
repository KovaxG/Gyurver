module Gyurver.Logger (Logger(..), info, error, warn, debug) where

import Prelude hiding (error)
import qualified Data.Time as Time

data Logger = Console | File

data LogMode = Info | Warning | Error | Debug

instance Show LogMode where
  show mode = case mode of
    Info    -> "INFO   "
    Error   -> "ERROR  "
    Warning -> "WARNING"
    Debug   -> "DEBUG  "

info :: Logger -> String -> IO ()
info = genericLog Info

warn :: Logger -> String -> IO ()
warn = genericLog Warning

error :: Logger -> String -> IO ()
error = genericLog Error

debug :: (Show a) => Logger -> a -> IO ()
debug logger a = genericLog Debug logger $ show a

genericLog :: LogMode -> Logger -> String -> IO ()
genericLog logMode logger message = do
  time <- Time.getZonedTime
  let msg = sbrackets (show logMode ++ " " ++ show time) ++ " " ++ message
  case logger of
    Console -> putStrLn msg
    File -> do
      let fileName = takeWhile (/= ' ') (show time) ++ ".gyurlog"
      appendFile fileName (msg ++ "\n")
      putStrLn msg

sbrackets :: String -> String
sbrackets s = "[" ++ s ++ "]"
