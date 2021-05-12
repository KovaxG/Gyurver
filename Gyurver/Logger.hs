module Gyurver.Logger (Logger(..), info, error, warn, debug) where

import Prelude hiding (error)
import           Control.Exception (SomeException)
import qualified Control.Exception as Exception
import qualified System.Directory as Dir
import qualified Data.Time as Time
import           Utils ((</>))

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
      let folderName = "logs"
      let fileName = takeWhile (/= ' ') (show time) ++ ".gyurlog"
      let path = folderName </> fileName
      let tryLogging = appendFile path (msg ++ "\n")
      Exception.catch
        tryLogging
        (\e -> do
          print (e :: SomeException)
          Dir.createDirectory folderName
          tryLogging
        )
      putStrLn msg

sbrackets :: String -> String
sbrackets s = "[" ++ s ++ "]"
