{-# LANGUAGE OverloadedStrings #-}

module Gyurver.Logger (Logger(..), info, error, warn, debug) where

import           Prelude hiding (error)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
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

info :: Logger -> Text -> IO ()
info = genericLog Info

warn :: Logger -> Text -> IO ()
warn = genericLog Warning

error :: Logger -> Text -> IO ()
error = genericLog Error

debug :: (Show a) => Logger -> a -> IO ()
debug logger a = genericLog Debug logger $ Text.pack $ show a

genericLog :: LogMode -> Logger -> Text -> IO ()
genericLog logMode logger message = do
  time <- Time.getZonedTime
  let msg = sbrackets (Text.pack (show logMode) <> " " <> Text.pack (show time)) <> " " <> message
  case logger of
    Console -> TIO.putStrLn msg
    File -> do
      let folderName = "logs" :: Text
      let fileName = Text.takeWhile (/= ' ') (Text.pack $ show time) <> ".gyurlog"
      let path = folderName </> fileName
      let tryLogging = TIO.appendFile (Text.unpack path) (msg <> "\n")
      Exception.catch
        tryLogging
        (\e -> do
          print (e :: SomeException)
          Dir.createDirectory (Text.unpack folderName)
          tryLogging
        )
      TIO.putStrLn msg

sbrackets :: Text -> Text
sbrackets s = "[" <> s <> "]"
