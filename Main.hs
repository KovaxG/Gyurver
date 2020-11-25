{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (log)

import qualified Component.Decoder as Decoder

import           Component.Database (DBHandle)
import qualified Component.Database as DB
import qualified Component.Json as Json

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function ((&))
import qualified Data.List as List

import Events.Cokkolo

import Gyurver.Html
import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
import           Gyurver.Logger (Logger(..))
import qualified Gyurver.Logger as Logger
import           Types.Video (Video)
import qualified Types.Video as Video
import qualified Types.VideoAdd as VideoAdd
import qualified Types.VideoEdit as VideoEdit
import Types.Password as Password
import Types.Settings as Settings
import Endpoints
import Utils (($>), safeReadBinaryFile, safeReadTextFile)

log :: Logger
log = File

main :: IO ()
main = do
  Logger.info log "Gyurver is starting..."

  tojasDB <- DB.getHandle "cokkolo2020"
  vidsDB <- DB.getHandle "vids"

  settings <- readSettings log
  Logger.info log (show settings)
  runServer log
            (settings & hostAddress)
            (settings & port)
            (process tojasDB vidsDB settings)

readSettings :: Logger -> IO Settings
readSettings log =
  safeReadTextFile "gyurver.settings" >>= maybe fileNotFound fileFound
  where
    fileNotFound :: IO Settings
    fileNotFound = do
      Logger.warn log "Could not read settings file, using default settings."
      return defaultSettings

    fileFound :: Text -> IO Settings
    fileFound contents = contents & Text.unpack & Settings.parse & either settingsParseFailed settingsLoaded

    settingsParseFailed :: String -> IO Settings
    settingsParseFailed msg = do
      let message = "Found settings file, but failed to parse it (" ++ msg ++ "), using default settings."
      Logger.error log message
      return defaultSettings

    settingsLoaded :: Settings -> IO Settings
    settingsLoaded settings = do
      Logger.info log $ "Loaded settings, ip is " ++ show (hostAddress settings)
      return settings

process :: DBHandle Tojas
        -> DBHandle Video
        -> Settings
        -> Request
        -> IO Response
process tojasDB
        vidsDB
        settings
        Request{requestType, path, content} =
  case parseEndpoint $ unwords [show requestType, path] of
    GetLandingPage -> do
      Logger.info log $ "Requested landing page, sending " ++ mainPath
      sendFile mainPath

    GetCV -> do
      Logger.info log $ "Requested CV."
      sendFile cvPath

    GetFavicon -> do
      Logger.info log $ "Requested favicon."
      sendFile faviconPath

    GetArticlesPage -> do
      Logger.info log $ "Requested articles page."
      sendFile mainPath

    GetCokkJSON -> do
      Logger.info log $ "[API] Requested cokkolesi lista."
      tojasok <- DB.everythingList tojasDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ tojasokToJson tojasok

    GetVideosPage -> do
      Logger.info log $ "Requested video list."
      sendFile mainPath

    GetVideosJSON -> do
      Logger.info log $ "[API] Requested video list."
      videos <- DB.everythingList vidsDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ Video.videosToJson videos

    GetVideoJSON reqNr -> do
      Logger.info log $ "[API] Requesting video with nr: " ++ show reqNr
      videos <- DB.everythingList vidsDB
      videos & List.find (\v -> Video.nr v == reqNr)
             & maybe badRequest (addHeaders [("Content-Type", "application/json")] . makeResponse OK . Video.videoToJson)
             & return

    GetCokkResultsPage -> do
      Logger.info log $ "Requested results."
      sendFile mainPath

    GetCokkPage -> do
      Logger.info log $ "Requested add egg page."
      sendFile mainPath

    GetVideosAddPage -> do
      Logger.info log $ "Requested video add page."
      sendFile mainPath

    GetResource resource -> do
      Logger.info log $ "Requesting resource [" ++ resource ++ "]."
      case parseResource resource of
        Just (Resource _ term) -> do
          let filePath = contentPath </> (term ++ "s") </> resource
          Logger.info log $ "Sending " ++ filePath ++"... Let's hope it exists..."
          sendFile filePath
        Nothing -> do
          Logger.warn log $ "No such resource: " ++ path
          return badRequest

    PostVideo -> do
      Logger.info log "[API] Adding new video to list."
      Json.parseJson content
        >>= Decoder.run VideoAdd.decoder
        & either
          (return . makeResponse BadRequest)
          (\request ->
            case VideoAdd.toVideo settings request of
              Right videoWithoutIndex -> DB.insertWithIndex vidsDB videoWithoutIndex Video.nr $> success
              Left error -> Logger.info log error $> makeResponse Unauthorized error
          )

    PostVideoJSON reqNr -> do
      Logger.info log $ "[API] Modified video with nr: " ++ show reqNr
      Json.parseJson content
        >>= Decoder.run VideoEdit.decoder
        & either
          (return . makeResponse BadRequest)
          (\request ->
            case VideoEdit.toVideo settings request of
              Right videoWithoutIndex -> DB.repsertWithIndex vidsDB (videoWithoutIndex reqNr) Video.nr $> success
              Left error -> Logger.info log error $> makeResponse Unauthorized error
          )

    DeleteVideoJSON reqNr -> do
      Logger.info log $ "[API] Delete video nr: " ++ show reqNr
      Json.parseJson content
        >>= Decoder.run Password.decoder
        & either
          (return . makeResponse BadRequest)
          (\(Password pwd) ->
            if pwd == (settings & password)
            then DB.delete vidsDB (\v -> Video.nr v == reqNr) $> success
            else Logger.info log ("Bad password: " ++ pwd) $> makeResponse Unauthorized ""
          )

    OptionsVideo -> do
      Logger.info log "Someone asked if you can post to /api/videos/new, sure."
      return allowHeaders

    OptionsVideoJSON reqNr -> do
      Logger.info log $ "Someone asked if you can post to /api/video/" ++ show reqNr ++ ", sure."
      return allowHeaders

    Other req -> do
      Logger.warn log $ "Weird request: " ++ req
      return badRequest

contentPath :: String
contentPath = "Content"

cvPath :: String
cvPath = contentPath </> "pdfs" </> "cv.pdf"

faviconPath :: String
faviconPath = contentPath </> "favicon.ico"

mainPath :: String
mainPath = contentPath </> "index.html"

allowHeaders :: Response
allowHeaders =
  "Wanna try posting stuff? Go ahead."
  & makeResponse OK
  & addHeaders
    [ ("Access-Control-Allow-Headers", "OPTIONS, POST")
    , ("Access-Control-Allow-Origin",  "*") -- Added to allow requests from localhost
    ]

badRequest :: Response
badRequest =
  makeResponse BadRequest
  $ Document
    [title [] [text "Gyurver"]]
    [h1 [] [text "Your request was bad, and you should feel bad. Nah, just messing with you, have a nice day, but your requests still suck tho."]]

sendFile :: String -> IO Response
sendFile path =
  safeReadBinaryFile path
  & fmap (maybe (makeResponse InternalServerError "Could not read file!") (makeResponse OK))

(</>) :: String -> String -> String
a </> b = a ++ "/" ++ b
