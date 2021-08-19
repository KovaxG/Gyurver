{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (log)

import qualified Component.Decoder as Decoder

import           Component.Database (DBHandle)
import qualified Component.Database as DB
import qualified Component.Json as Json

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Function ((&))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Monoid ((<>))

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
import           Types.Blog (Blog, Index(..))
import qualified Types.Blog as Blog
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
  Logger.info log "Caching main file..."
  fileResponse <- sendFile mainPath

  Logger.info log "Gyurver is starting..."

  tojasDB <- DB.getHandle "cokkolo2020"
  vidsDB <- DB.getHandle "vids"
  movieDiffDB <- DB.getHandle "movieDiff"
  suggestionBoxDB <- DB.getHandle "suggestionBox"
  blogDB <- DB.getHandle "blogLookup"

  cokk2021UserDB <- DB.getHandle "cokk2021User"
  cokk2021WaterDB <- DB.getHandle "cokk2021Water"
  cokk2021ItemDB <- DB.getHandle "cokk2021Item"

  settings <- readSettings log
  Logger.info log (Text.pack $ show settings)
  Server.run log
            (settings & Settings.hostAddress)
            (settings & Settings.port)
            (process fileResponse tojasDB vidsDB cokk2021UserDB cokk2021WaterDB cokk2021ItemDB suggestionBoxDB movieDiffDB blogDB settings)

readSettings :: Logger -> IO Settings
readSettings log =
  Utils.safeReadTextFile "gyurver.settings" >>= maybe fileNotFound fileFound
  where
    fileNotFound :: IO Settings
    fileNotFound = do
      Logger.warn log "Could not read settings file, using default settings."
      return Settings.defaultSettings

    fileFound :: Text -> IO Settings
    fileFound contents = contents & Settings.parse & either settingsParseFailed settingsLoaded

    settingsParseFailed :: Text -> IO Settings
    settingsParseFailed msg = do
      let message = "Found settings file, but failed to parse it (" <> msg <> "), using default settings."
      Logger.error log message
      return Settings.defaultSettings

    settingsLoaded :: Settings -> IO Settings
    settingsLoaded settings = do
      Logger.info log $ "Loaded settings, ip is " <> Text.pack (show (Settings.hostAddress settings))
      return settings

process :: Response
        -> DBHandle Cokk2020.Tojas
        -> DBHandle Video
        -> DBHandle Cokk2021User.User
        -> DBHandle Cokk2021WaterLog.WaterLog
        -> DBHandle Cokk2021Item.Item
        -> DBHandle Text
        -> DBHandle Movie.MovieDiff
        -> DBHandle Blog.Index
        -> Settings
        -> Request
        -> IO Response
process mainFile
        tojasDB
        vidsDB
        cokk2021UserDB
        cokk2021WaterDB
        cokk2021ItemDB
        suggestionBoxDB
        movieDiffDB
        blogDB
        settings
        Request{requestType, path, content} = do
  let
    movieProcessing :: (Text -> MovieDiff) -> Text -> IO Response
    movieProcessing diff successMsg =
      if not (Text.null content)
      then do
        DB.insert movieDiffDB $ diff $ Utils.dequote content
        Response.make OK successMsg
      else
        Response.make BadRequest ("I need the name of the film in the body!" :: Text)

  case Endpoint.parse $ Text.unwords [Text.pack $ show requestType, path] of
    Endpoint.GetLandingPage -> do
      Logger.info log $ "Requested landing page, sending " <> mainPath
      return mainFile

    Endpoint.GetCV -> do
      Logger.info log "Requested CV."
      sendFile cvPath

    Endpoint.GetFavicon -> do
      Logger.info log "Requested favicon."
      sendFile faviconPath

    Endpoint.GetArticlesPage -> do
      Logger.info log "Requested articles page."
      return mainFile

    Endpoint.GetBlogPage -> do
      Logger.info log "Requested blog page."
      return mainFile

    Endpoint.GetBlogItemPage blogNr -> do
      Logger.info log $ "Requested blog with index " <> Text.pack (show blogNr)
      return mainFile

    Endpoint.GetBlogItemsJSON -> do
      Logger.info log "Requested blog items"
      indexes <- DB.everythingList blogDB
      blogItems <- Maybe.mapMaybe Utils.eitherToMaybe <$> traverse (\(Index i s) -> Blog.readGyurblog i (blogPath s)) indexes
      Response.make OK (map Blog.toBlogItem blogItems)

    Endpoint.GetBlogJSON blogNr -> do
      Logger.info log $ "Requested blog nr " <> Text.pack (show blogNr)
      indexes <- DB.everythingList blogDB
      let fileNameMaybe = Blog.getFileName <$> List.find (Blog.withIndex blogNr) indexes
      maybe
        (Response.make NotFound $ "I have no blog with index " <> Text.pack (show blogNr))
        (\fileName -> do
          blog <- Blog.readGyurblog blogNr (blogPath fileName)
          either
            (const $ Response.make NotFound $ "I have no blog with index " <> Text.pack (show blogNr) <> ". Missing file!")
            (Response.make OK . Blog.toJson)
            blog
        )
        fileNameMaybe

    Endpoint.GetCokk2020JSON -> do
      Logger.info log "[API] Requested cokkolesi lista."
      tojasok <- DB.everythingList tojasDB
      Response.make OK (map Cokk2020.tojasToJson tojasok)

    Endpoint.GetVideosPage -> do
      Logger.info log "Requested video list."
      return mainFile

    Endpoint.GetVideosJSON -> do
      Logger.info log "[API] Requested video list."
      videos <- DB.everythingList vidsDB
      Response.make OK (Video.videosToJson videos)

    Endpoint.GetVideoJSON reqNr -> do
      Logger.info log $ "[API] Requesting video with nr: " <> Text.pack (show reqNr)
      videos <- DB.everythingList vidsDB
      videos & List.find (\v -> Video.nr v == reqNr)
             & maybe badRequest (Response.make OK . Video.videoToJson)

    Endpoint.GetCokk2020ResultsPage -> do
      Logger.info log "Requested results for 2020 Cokk."
      return mainFile

    Endpoint.GetCokk2021ResultsPage -> do
      Logger.info log "Requested results for 2021 Cokk."
      return mainFile

    Endpoint.GetCokk2020Page -> do
      Logger.info log "Requested cokk 2020 page."
      return mainFile

    Endpoint.GetCokk2021Page -> do
      Logger.info log "Requested cokk 2021 page"
      return mainFile

    Endpoint.GetCokk2021Participants ->
      Cokk2021Handler.getParticipants cokk2021UserDB cokk2021WaterDB

    Endpoint.PostSuggestion -> do
      Logger.info log "New suggestion!"
      DB.insert suggestionBoxDB $ "---\n" <> content
      Response.success

    Endpoint.PostCokk2021ParticipantsForUser ->
      Cokk2021Handler.getParticipantsForUser content cokk2021UserDB cokk2021WaterDB

    Endpoint.PostCokk2021Fight ->
      Cokk2021Handler.fight content cokk2021UserDB log

    Endpoint.GetVideosAddPage -> do
      Logger.info log "Requested video add page."
      return mainFile

    Endpoint.GetResource resource -> do
      Logger.info log $ "Requesting resource [" <> resource <> "]."
      case Endpoint.parseResource resource of
        Just (Endpoint.Resource _ term) -> do
          let filePath = contentPath </> (term <> "s") </> resource
          Logger.info log $ "Sending " <> filePath <> "... Let's hope it exists..."
          sendFile filePath
        Nothing -> do
          Logger.warn log $ "No such resource: " <> path
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
      Logger.info log $ "[API] Modified video with nr: " <> Text.pack (show reqNr)
      Response.processJsonBody content VideoEdit.decoder $ \video ->
        case VideoEdit.toVideo settings video of
          Right videoWithoutIndex -> do
            DB.repsertWithIndex vidsDB (videoWithoutIndex reqNr) Video.nr
            Response.success
          Left error -> do
            Logger.info log error
            Response.make Unauthorized error

    Endpoint.PostCokk2021Login ->
      Cokk2021Handler.login content cokk2021UserDB cokk2021WaterDB log

    Endpoint.PostCokk2021Register ->
      Cokk2021Handler.register content cokk2021UserDB settings log

    Endpoint.PostCokk2021Water ->
      Cokk2021Handler.water content cokk2021UserDB cokk2021WaterDB log settings

    Endpoint.PostCokk2021DashboardRefresh ->
      Cokk2021Handler.refreshDashboard content cokk2021UserDB cokk2021WaterDB

    Endpoint.PostCokk2021IncSkill ->
      Cokk2021Handler.incSkill content cokk2021UserDB log settings

    Endpoint.PostCokk2021ChangeEggname -> do
      Logger.info log $ "[API] change egg name with body: " <> content
      if (settings & Settings.cokk2021) == Types.Blocked
      then do
        Logger.info log "Event is locked!"
        Response.make Forbidden ("Event is locked!" :: Text)
      else do
        Response.processJsonBody content Cokk2021ChangeEggnameRequest.decode $ \req -> do
          users <- DB.everythingList cokk2021UserDB
          let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021ChangeEggnameRequest.toLogin req) users
          maybe
            (Response.make Unauthorized ("Bad Credentials" :: Text))
            (\user -> do
              let eggs = map Cokk2021User.eggname users
              let newEggname = Cokk2021ChangeEggnameRequest.newEggname req
              if newEggname `elem` eggs
              then Response.make Forbidden ("Egg already exists!" :: Text)
              else do
                DB.modifyData cokk2021UserDB
                  $ (, ()) . Utils.mapIf
                    (\u -> Cokk2021User.username u == Cokk2021User.username user)
                    (\u -> u { Cokk2021User.eggname = Cokk2021ChangeEggnameRequest.newEggname req })
                Response.make OK ("Ok Boomer" :: Text)
            )
            userOpt

    Endpoint.PostCokk2021BuyItem -> do
      Logger.info log $ "[API] buy request with body: " <> content

      if (settings & Settings.cokk2021) == Types.Blocked
      then do
        Logger.info log "Event is locked!"
        Response.make Forbidden ("Event is locked!" :: Text)
      else do
        Response.processJsonBody content Cokk2021ItemRequest.decode $ \req -> do
          users <- DB.everythingList cokk2021UserDB
          let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021ItemRequest.toLogin req) users
          maybe
            (Response.make Unauthorized ("Bad Credentials" :: Text))
            (\user -> do
              items <- DB.everythingList cokk2021ItemDB
              let itemOpt = List.find (\i -> Cokk2021Item.index i == Cokk2021ItemRequest.index req) items
              maybe
                (Response.make BadRequest ("This item does not exist!" :: Text))
                (\item -> do
                  if Cokk2021Item.index item `elem` Cokk2021User.items user
                  then Response.make BadRequest ("Item is already owned!" :: Text)
                  else if Cokk2021User.perfume user < Cokk2021Item.cost item
                  then Response.make PaymentRequired ("Not enough perfume" :: Text)
                  else do
                    DB.modifyData cokk2021UserDB
                      $ (, ()) . Utils.mapIf
                        (\u -> Cokk2021User.username u == Cokk2021User.username user)
                        (\u -> u { Cokk2021User.perfume = Cokk2021User.perfume user - Cokk2021Item.cost item
                                , Cokk2021User.items = Cokk2021Item.index item : Cokk2021User.items user
                                , Cokk2021User.base = item
                                }
                        )
                    Response.make OK ("Ok Boomer" :: Text)
                )
                itemOpt
            )
            userOpt

    Endpoint.PostCokk2021EquipItem -> do
      Logger.info log $ "[API] requested equip item endpoint with content: " <> content

      if (settings & Settings.cokk2021) == Types.Blocked
      then do
        Logger.info log "Event is locked!"
        Response.make Forbidden ("Event is locked!" :: Text)
      else do
        Response.processJsonBody content Cokk2021ItemRequest.decode $ \req -> do
          users <- DB.everythingList cokk2021UserDB
          let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021ItemRequest.toLogin req) users
          maybe
            (Response.make Unauthorized ("Bad Credentials" :: Text))
            (\user -> do
              items <- DB.everythingList cokk2021ItemDB
              let itemOpt = List.find (\i -> Cokk2021Item.index i == Cokk2021ItemRequest.index req) items
              maybe
                (Response.make BadRequest ("This item does not exist!" :: Text))
                (\item -> do
                  if Cokk2021Item.index item == Cokk2021Item.index (Cokk2021User.base user)
                  then Response.make OK ("The item is already equiped, but Ok." :: Text)
                  else do
                    DB.modifyData cokk2021UserDB
                      $ (, ()) . Utils.mapIf
                        (\u -> Cokk2021User.username u == Cokk2021User.username user)
                        (\u -> u { Cokk2021User.base = item })
                    Response.make OK ("Ok Boomer" :: Text)
                )
                itemOpt
            )
            userOpt

    Endpoint.DeleteVideoJSON reqNr -> do
      Logger.info log $ "[API] Delete video nr: " <> Text.pack (show reqNr)
      Response.processJsonBody content Password.decoder $ \(Password pwd) ->
        if pwd == (settings & Settings.password)
        then do
          DB.delete vidsDB (\v -> Video.nr v == reqNr)
          Response.success
        else do
          Logger.info log ("Bad password: " <> pwd)
          Response.make Unauthorized ("Bad password!" :: Text)

    Endpoint.OptionsVideo -> do
      Logger.info log "Someone asked if you can post to /api/videos/new, sure."
      allowHeaders

    Endpoint.OptionsVideoJSON reqNr -> do
      Logger.info log $ "Someone asked if you can post to /api/video/" <> Text.pack (show reqNr) <> ", sure."
      allowHeaders

    Endpoint.Film operation ->
      case operation of
        Endpoint.Insert -> movieProcessing Movie.NewMovie "added (if doesn't exist)"
        Endpoint.Modify -> movieProcessing (`Movie.SetWatched` True) "marked as watched (if exists)"
        Endpoint.Delete -> movieProcessing Movie.Delete "removed (if exists)"
        Endpoint.Obtain -> do
          movieDiffs <- DB.everythingList movieDiffDB
          let movies = Movie.combineDiffs movieDiffs
          Response.make OK (Movie.toJson movies)

    Endpoint.Other req -> do
      Logger.warn log $ "Weird request: " <> req
      badRequest

contentPath :: Text
contentPath = "Content"

cvPath :: Text
cvPath = contentPath </> "pdfs" </> "cv.pdf"

faviconPath :: Text
faviconPath = contentPath </> "favicon.ico"

mainPath :: Text
mainPath = contentPath </> "index.html"

blogPath :: Text -> Text
blogPath s = "Content" </> "gyurblogs" </> s

allowHeaders :: IO Response
allowHeaders =
  Response.addHeaders
    [ ("Access-Control-Allow-Headers", "OPTIONS, POST")
    , ("Access-Control-Allow-Origin",  "*") -- Added to allow requests from localhost
    ] <$> Response.make OK ("Wanna try posting stuff? Go ahead." :: Text)

badRequest :: IO Response
badRequest =
  Response.make BadRequest
  $ Document
    [title [] [text "Gyurver"]]
    [h1 [] [text "Your request was bad, and you should feel bad. Nah, just messing with you, have a nice day, but your requests still suck tho."]]

sendFile :: Text -> IO Response
sendFile path = do
  contentOpt <- Utils.safeReadBinaryFile (Text.unpack path)
  maybe
    (Response.make InternalServerError ("Could not read file!" :: Text))
    (Response.make OK)
    contentOpt
