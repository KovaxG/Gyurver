{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
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
import qualified Data.Maybe as Maybe

import qualified Events.Cokk2020 as Cokk2020
import qualified Events.Cokk2021.Handlers as Cokk2021Handler
import qualified Events.Cokk2021.User as Cokk2021User
import qualified Events.Cokk2021.Item as Cokk2021Item
import qualified Events.Cokk2021.Login as Cokk2021Login
import qualified Events.Cokk2021.Skills as Cokk2021Skills
import qualified Events.Cokk2021.WaterLog as Cokk2021WaterLog
import qualified Events.Cokk2021.ItemRequest as Cokk2021ItemRequest
import qualified Events.Cokk2021.Registration as Cokk2021Registration
import qualified Events.Cokk2021.WaterRequest as Cokk2021WaterRequest
import qualified Events.Cokk2021.FightRequest as Cokk2021FightRequest
import qualified Events.Cokk2021.DashboardData as Cokk2021DashboardData
import qualified Events.Cokk2021.IncSkillRequest as Cokk2021IncSkillRequest
import qualified Events.Cokk2021.ChangeEggnameRequest as Cokk2021ChangeEggnameRequest

import Gyurver.Html
import Gyurver.Request
import qualified Gyurver.Response as Response
import           Gyurver.Response (Response, Status(..))
import qualified Gyurver.Server as Server
import           Gyurver.Logger (Logger(..))
import qualified Gyurver.Logger as Logger
import qualified Types.Common as Types
import qualified Types.DateTime as DateTime
import           Types.Movie (Movie, MovieDiff)
import qualified Types.Movie as Movie
import           Types.Video (Video)
import qualified Types.Video as Video
import qualified Types.VideoAdd as VideoAdd
import qualified Types.VideoEdit as VideoEdit
import           Types.Password as Password
import           Types.Settings (Settings)
import qualified Types.Settings as Settings
import qualified Endpoints as Endpoint
import           Utils (($>), (</>))
import qualified Utils

log :: Logger
log = File

main :: IO ()
main = do
  Logger.info log "Gyurver is starting..."

  tojasDB <- DB.getHandle "cokkolo2020"
  vidsDB <- DB.getHandle "vids"
  suggestionBoxDB <- DB.getHandle "suggestionBox"

  cokk2021UserDB <- DB.getHandle "cokk2021User"
  cokk2021WaterDB <- DB.getHandle "cokk2021Water"
  cokk2021ItemDB <- DB.getHandle "cokk2021Item"

  movieDiffDB <- DB.getHandle "movieDiff"

  settings <- readSettings log
  Logger.info log (show settings)
  Server.run log
            (settings & Settings.hostAddress)
            (settings & Settings.port)
            (process tojasDB vidsDB cokk2021UserDB cokk2021WaterDB cokk2021ItemDB suggestionBoxDB movieDiffDB settings)

readSettings :: Logger -> IO Settings
readSettings log =
  Utils.safeReadTextFile "gyurver.settings" >>= maybe fileNotFound fileFound
  where
    fileNotFound :: IO Settings
    fileNotFound = do
      Logger.warn log "Could not read settings file, using default settings."
      return Settings.defaultSettings

    fileFound :: Text -> IO Settings
    fileFound contents = contents & Text.unpack & Settings.parse & either settingsParseFailed settingsLoaded

    settingsParseFailed :: String -> IO Settings
    settingsParseFailed msg = do
      let message = "Found settings file, but failed to parse it (" ++ msg ++ "), using default settings."
      Logger.error log message
      return Settings.defaultSettings

    settingsLoaded :: Settings -> IO Settings
    settingsLoaded settings = do
      Logger.info log $ "Loaded settings, ip is " ++ show (Settings.hostAddress settings)
      return settings

process :: DBHandle Cokk2020.Tojas
        -> DBHandle Video
        -> DBHandle Cokk2021User.User
        -> DBHandle Cokk2021WaterLog.WaterLog
        -> DBHandle Cokk2021Item.Item
        -> DBHandle String
        -> DBHandle Movie.MovieDiff
        -> Settings
        -> Request
        -> IO Response
process tojasDB
        vidsDB
        cokk2021UserDB
        cokk2021WaterDB
        cokk2021ItemDB
        suggestionBoxDB
        movieDiffDB
        settings
        Request{requestType, path, content} = do
  let
    movieProcessing :: (String -> MovieDiff) -> String -> IO Response
    movieProcessing diff successMsg =
      if not (null content)
      then do
        DB.insert movieDiffDB $ diff $ Utils.dequote content
        Response.make OK successMsg
      else
        Response.make BadRequest "I need the name of the film in the body!"

  case Endpoint.parse $ unwords [show requestType, path] of
    Endpoint.GetLandingPage -> do
      Logger.info log $ "Requested landing page, sending " ++ mainPath
      sendFile mainPath

    Endpoint.GetCV -> do
      Logger.info log "Requested CV."
      sendFile cvPath

    Endpoint.GetFavicon -> do
      Logger.info log "Requested favicon."
      sendFile faviconPath

    Endpoint.GetArticlesPage -> do
      Logger.info log "Requested articles page."
      sendFile mainPath

    Endpoint.GetCokk2020JSON -> do
      Logger.info log "[API] Requested cokkolesi lista."
      tojasok <- DB.everythingList tojasDB
      Response.addHeaders [("Content-Type", "application/json")]
        <$> Response.make OK (map Cokk2020.tojasToJson tojasok)

    Endpoint.GetVideosPage -> do
      Logger.info log "Requested video list."
      sendFile mainPath

    Endpoint.GetVideosJSON -> do
      Logger.info log "[API] Requested video list."
      videos <- DB.everythingList vidsDB
      Response.addHeaders [("Content-Type", "application/json")]
        <$> Response.make OK (Video.videosToJson videos)

    Endpoint.GetVideoJSON reqNr -> do
      Logger.info log $ "[API] Requesting video with nr: " ++ show reqNr
      videos <- DB.everythingList vidsDB
      videos & List.find (\v -> Video.nr v == reqNr)
             & maybe badRequest (\v ->
               Response.addHeaders [("Content-Type", "application/json")]
               <$> Response.make OK (Video.videoToJson v)
             )

    Endpoint.GetCokk2020ResultsPage -> do
      Logger.info log "Requested results for 2020 Cokk."
      sendFile mainPath

    Endpoint.GetCokk2021ResultsPage -> do
      Logger.info log "Requested results for 2021 Cokk."
      sendFile mainPath

    Endpoint.GetCokk2020Page -> do
      Logger.info log "Requested cokk 2020 page."
      sendFile mainPath

    Endpoint.GetCokk2021Page -> do
      Logger.info log "Requested cokk 2021 page"
      sendFile mainPath

    Endpoint.GetCokk2021Participants ->
      Cokk2021Handler.getParticipants cokk2021UserDB cokk2021WaterDB

    Endpoint.PostSuggestion -> do
      Logger.info log "New suggestion!"
      DB.insert suggestionBoxDB $ "---\n" ++ content
      Response.success

    Endpoint.PostCokk2021ParticipantsForUser -> do
      Cokk2021Handler.getParticipantsForUser content cokk2021UserDB cokk2021WaterDB

    Endpoint.PostCokk2021Fight -> do
      Cokk2021Handler.fight content cokk2021UserDB log

    Endpoint.GetVideosAddPage -> do
      Logger.info log "Requested video add page."
      sendFile mainPath

    Endpoint.GetResource resource -> do
      Logger.info log $ "Requesting resource [" ++ resource ++ "]."
      case Endpoint.parseResource resource of
        Just (Endpoint.Resource _ term) -> do
          let filePath = contentPath </> (term ++ "s") </> resource
          Logger.info log $ "Sending " ++ filePath ++"... Let's hope it exists..."
          sendFile filePath
        Nothing -> do
          Logger.warn log $ "No such resource: " ++ path
          badRequest

    Endpoint.GetCokk2021Items -> do
      Cokk2021Handler.getItems cokk2021ItemDB

    Endpoint.PostVideo -> do
      Logger.info log "[API] Adding new video to list."
      Response.processJsonBody content VideoAdd.decoder $ \request ->
        case VideoAdd.toVideo settings request of
          Right videoWithoutIndex -> do
            DB.insertWithIndex vidsDB videoWithoutIndex Video.nr
            Response.success
          Left error -> do
            Logger.info log error
            Response.make Unauthorized error

    Endpoint.PostVideoJSON reqNr -> do
      Logger.info log $ "[API] Modified video with nr: " ++ show reqNr
      Response.processJsonBody content VideoEdit.decoder $ \video ->
        case VideoEdit.toVideo settings video of
          Right videoWithoutIndex -> do
            DB.repsertWithIndex vidsDB (videoWithoutIndex reqNr) Video.nr
            Response.success
          Left error -> do
            Logger.info log error
            Response.make Unauthorized error

    Endpoint.PostCokk2021Login -> do
      Cokk2021Handler.login content cokk2021UserDB cokk2021WaterDB log

    Endpoint.PostCokk2021Register -> do
      Cokk2021Handler.register content cokk2021UserDB settings log

    Endpoint.PostCokk2021Water -> do
      Cokk2021Handler.water content cokk2021UserDB cokk2021WaterDB log settings

    Endpoint.PostCokk2021DashboardRefresh -> do
      Cokk2021Handler.refreshDashboard content cokk2021UserDB cokk2021WaterDB

    Endpoint.PostCokk2021IncSkill -> do
      Cokk2021Handler.incSkill content cokk2021UserDB log settings

    Endpoint.PostCokk2021ChangeEggname -> do
      Logger.info log $ "[API] change egg name with body: " ++ content
      if (settings & Settings.cokk2021) == Types.Blocked
      then do
        Logger.info log "Event is locked!"
        Response.make Forbidden "Event is locked!"
      else do
        Response.processJsonBody content Cokk2021ChangeEggnameRequest.decode $ \req -> do
          users <- DB.everythingList cokk2021UserDB
          let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021ChangeEggnameRequest.toLogin req) users
          maybe
            (Response.make Unauthorized "Bad Credentials")
            (\user -> do
              let eggs = map Cokk2021User.eggname users
              let newEggname = Cokk2021ChangeEggnameRequest.newEggname req
              if newEggname `elem` eggs
              then Response.make Forbidden "Egg already exists!"
              else do
                DB.modifyData cokk2021UserDB
                  $ (, ()) . Utils.mapIf
                    (\u -> Cokk2021User.username u == Cokk2021User.username user)
                    (\u -> u { Cokk2021User.eggname = Cokk2021ChangeEggnameRequest.newEggname req })
                Response.make OK "Ok Boomer"
            )
            userOpt

    Endpoint.PostCokk2021BuyItem -> do
      Logger.info log $ "[API] buy request with body: " ++ content

      if (settings & Settings.cokk2021) == Types.Blocked
      then do
        Logger.info log "Event is locked!"
        Response.make Forbidden "Event is locked!"
      else do
        Response.processJsonBody content Cokk2021ItemRequest.decode $ \req -> do
          users <- DB.everythingList cokk2021UserDB
          let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021ItemRequest.toLogin req) users
          maybe
            (Response.make Unauthorized "Bad Credentials")
            (\user -> do
              items <- DB.everythingList cokk2021ItemDB
              let itemOpt = List.find (\i -> Cokk2021Item.index i == Cokk2021ItemRequest.index req) items
              maybe
                (Response.make BadRequest "This item does not exist!")
                (\item -> do
                  if Cokk2021Item.index item `elem` Cokk2021User.items user
                  then Response.make BadRequest "Item is already owned!"
                  else if Cokk2021User.perfume user < Cokk2021Item.cost item
                  then Response.make PaymentRequired "Not enough perfume"
                  else do
                    DB.modifyData cokk2021UserDB
                      $ (, ()) . Utils.mapIf
                        (\u -> Cokk2021User.username u == Cokk2021User.username user)
                        (\u -> u { Cokk2021User.perfume = Cokk2021User.perfume user - Cokk2021Item.cost item
                                , Cokk2021User.items = Cokk2021Item.index item : Cokk2021User.items user
                                , Cokk2021User.base = item
                                }
                        )
                    Response.make OK "Ok Boomer"
                )
                itemOpt
            )
            userOpt

    Endpoint.PostCokk2021EquipItem -> do
      Logger.info log $ "[API] requested equip item endpoint with content: " ++ content

      if (settings & Settings.cokk2021) == Types.Blocked
      then do
        Logger.info log "Event is locked!"
        Response.make Forbidden "Event is locked!"
      else do
        Response.processJsonBody content Cokk2021ItemRequest.decode $ \req -> do
          users <- DB.everythingList cokk2021UserDB
          let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021ItemRequest.toLogin req) users
          maybe
            (Response.make Unauthorized "Bad Credentials")
            (\user -> do
              items <- DB.everythingList cokk2021ItemDB
              let itemOpt = List.find (\i -> Cokk2021Item.index i == Cokk2021ItemRequest.index req) items
              maybe
                (Response.make BadRequest "This item does not exist!")
                (\item -> do
                  if Cokk2021Item.index item == Cokk2021Item.index (Cokk2021User.base user)
                  then Response.make OK "The item is already equiped, but Ok."
                  else do
                    DB.modifyData cokk2021UserDB
                      $ (, ()) . Utils.mapIf
                        (\u -> Cokk2021User.username u == Cokk2021User.username user)
                        (\u -> u { Cokk2021User.base = item })
                    Response.make OK "Ok Boomer"
                )
                itemOpt
            )
            userOpt

    Endpoint.DeleteVideoJSON reqNr -> do
      Logger.info log $ "[API] Delete video nr: " ++ show reqNr
      Response.processJsonBody content Password.decoder $ \(Password pwd) ->
        if pwd == (settings & Settings.password)
        then do
          DB.delete vidsDB (\v -> Video.nr v == reqNr)
          Response.success
        else do
          Logger.info log ("Bad password: " ++ pwd)
          Response.make Unauthorized "Bad password!"

    Endpoint.OptionsVideo -> do
      Logger.info log "Someone asked if you can post to /api/videos/new, sure."
      allowHeaders

    Endpoint.OptionsVideoJSON reqNr -> do
      Logger.info log $ "Someone asked if you can post to /api/video/" ++ show reqNr ++ ", sure."
      allowHeaders

    Endpoint.Film operation ->
      case operation of
        Endpoint.Insert -> movieProcessing Movie.NewMovie "added (if doesn't exist)"
        Endpoint.Modify -> movieProcessing (`Movie.SetWatched` True) "marked as watched (if exists)"
        Endpoint.Delete -> movieProcessing Movie.Delete "removed (if exists)"
        Endpoint.Obtain -> do
          movieDiffs <- DB.everythingList movieDiffDB
          let movies = Movie.combineDiffs movieDiffs
          Response.make OK $ Movie.toJson movies

    Endpoint.Other req -> do
      Logger.warn log $ "Weird request: " ++ req
      badRequest

contentPath :: String
contentPath = "Content"

cvPath :: String
cvPath = contentPath </> "pdfs" </> "cv.pdf"

faviconPath :: String
faviconPath = contentPath </> "favicon.ico"

mainPath :: String
mainPath = contentPath </> "index.html"

allowHeaders :: IO Response
allowHeaders =
  Response.addHeaders
    [ ("Access-Control-Allow-Headers", "OPTIONS, POST")
    , ("Access-Control-Allow-Origin",  "*") -- Added to allow requests from localhost
    ] <$> Response.make OK "Wanna try posting stuff? Go ahead."

badRequest :: IO Response
badRequest =
  Response.make BadRequest
  $ Document
    [title [] [text "Gyurver"]]
    [h1 [] [text "Your request was bad, and you should feel bad. Nah, just messing with you, have a nice day, but your requests still suck tho."]]

sendFile :: String -> IO Response
sendFile path = do
  contentOpt <- Utils.safeReadBinaryFile path
  maybe
    (Response.make InternalServerError "Could not read file!")
    (Response.make OK)
    contentOpt
