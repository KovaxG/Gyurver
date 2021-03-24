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
import qualified Events.Cokk2021.User as Cokk2021User
import qualified Events.Cokk2021.Item as Cokk2021Item
import qualified Events.Cokk2021.Login as Cokk2021Login
import qualified Events.Cokk2021.Skills as Cokk2021Skills
import qualified Events.Cokk2021.WaterLog as Cokk2021WaterLog
import qualified Events.Cokk2021.Registration as Cokk2021Registration
import qualified Events.Cokk2021.WaterRequest as Cokk2021WaterRequest
import qualified Events.Cokk2021.DashboardData as Cokk2021DashboardData
import qualified Events.Cokk2021.IncSkillRequest as Cokk2021IncSkillRequest
import qualified Events.Cokk2021.BuyItemRequest as Cokk2021BuyItemRequest
import qualified Events.Cokk2021.ChangeEggnameRequest as Cokk2021ChangeEggnameRequest

import Gyurver.Html
import Gyurver.Request
import Gyurver.Response
import Gyurver.Server
import           Gyurver.Logger (Logger(..))
import qualified Gyurver.Logger as Logger
import qualified Types.DateTime as DateTime
import           Types.Video (Video)
import qualified Types.Video as Video
import qualified Types.VideoAdd as VideoAdd
import qualified Types.VideoEdit as VideoEdit
import           Types.Password as Password
import           Types.Settings as Settings
import Endpoints
import           Utils (($>), (</>))
import qualified Utils

log :: Logger
log = File

main :: IO ()
main = do
  Logger.info log "Gyurver is starting..."

  tojasDB <- DB.getHandle "cokkolo2020"
  vidsDB <- DB.getHandle "vids"

  cokk2021UserDB <- DB.getHandle "cokk2021User"
  cokk2021WaterDB <- DB.getHandle "cokk2021Water"
  cokk2021ItemDB <- DB.getHandle "cokk2021Item"

  settings <- readSettings log
  Logger.info log (show settings)
  runServer log
            (settings & hostAddress)
            (settings & port)
            (process tojasDB vidsDB cokk2021UserDB cokk2021WaterDB cokk2021ItemDB settings)

readSettings :: Logger -> IO Settings
readSettings log =
  Utils.safeReadTextFile "gyurver.settings" >>= maybe fileNotFound fileFound
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

process :: DBHandle Cokk2020.Tojas
        -> DBHandle Video
        -> DBHandle Cokk2021User.User
        -> DBHandle Cokk2021WaterLog.WaterLog
        -> DBHandle Cokk2021Item.Item
        -> Settings
        -> Request
        -> IO Response
process tojasDB
        vidsDB
        cokk2021UserDB
        cokk2021WaterDB
        cokk2021ItemDB
        settings
        Request{requestType, path, content} = do
  let
    processJsonBody :: Decoder.Decoder a -> (a -> IO Response) -> IO Response
    processJsonBody decoder handle =
      Json.parseJson content >>= Decoder.run decoder & either (return . makeResponse BadRequest) handle

  case parseEndpoint $ unwords [show requestType, path] of
    GetLandingPage -> do
      Logger.info log $ "Requested landing page, sending " ++ mainPath
      sendFile mainPath

    GetCV -> do
      Logger.info log "Requested CV."
      sendFile cvPath

    GetFavicon -> do
      Logger.info log "Requested favicon."
      sendFile faviconPath

    GetArticlesPage -> do
      Logger.info log "Requested articles page."
      sendFile mainPath

    GetCokk2020JSON -> do
      Logger.info log "[API] Requested cokkolesi lista."
      tojasok <- DB.everythingList tojasDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ map Cokk2020.tojasToJson tojasok

    GetVideosPage -> do
      Logger.info log "Requested video list."
      sendFile mainPath

    GetVideosJSON -> do
      Logger.info log "[API] Requested video list."
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

    GetCokk2020ResultsPage -> do
      Logger.info log "Requested results."
      sendFile mainPath

    GetCokk2020Page -> do
      Logger.info log "Requested cokk 2020 page."
      sendFile mainPath

    GetCokk2021Page -> do
      Logger.info log "Requested cokk 2021 page"
      sendFile mainPath

    GetCokk2021Participants  -> do
      Logger.info log "[API] Requested participants list"
      users <- DB.everythingList cokk2021UserDB
      return
        $ addHeaders [("Content-Type", "application/json")]
        $ makeResponse OK
        $ map (Cokk2021User.toListItemJson True) users

    PostCokk2021ParticipantsForUser -> do
      Logger.info log "[API] Requested user participation list"
      processJsonBody Cokk2021Login.decode $ \login -> do
        users <- DB.everythingList cokk2021UserDB
        let userOpt = List.find (Cokk2021Login.matchesLogin login) users
        maybe
          (return $ makeResponse Unauthorized "Bad Credentials")
          (\user -> do
            waterLogs <- DB.everythingList cokk2021WaterDB
            now <- DateTime.getCurrentDateTime
            let relevantLines = filter (\w -> Cokk2021WaterLog.wlSource w == Cokk2021User.username user) waterLogs
            let nusers =
                  map (\u ->
                    ( maybe True (flip Cokk2021WaterRequest.isWaterable now . Cokk2021WaterLog.wlDateTime)
                        $ Utils.safeLast
                        $ List.sortOn Cokk2021WaterLog.wlDateTime
                        $ filter (\w -> Cokk2021WaterLog.wlTarget w == Cokk2021User.username u) relevantLines
                    , u
                    )
                  ) users
            return
              $ addHeaders [("Content-Type", "application/json")]
              $ makeResponse OK
              $ map (uncurry Cokk2021User.toListItemJson) nusers
          )
          userOpt

    GetVideosAddPage -> do
      Logger.info log "Requested video add page."
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

    GetCokk2021Items -> do
      Logger.info log "[API] Requested items"
      items <- DB.everythingList cokk2021ItemDB
      return $ makeResponse OK $ map Cokk2021Item.encode items

    PostVideo -> do
      Logger.info log "[API] Adding new video to list."
      processJsonBody VideoAdd.decoder $ \request ->
        case VideoAdd.toVideo settings request of
          Right videoWithoutIndex -> DB.insertWithIndex vidsDB videoWithoutIndex Video.nr $> success
          Left error -> Logger.info log error $> makeResponse Unauthorized error


    PostVideoJSON reqNr -> do
      Logger.info log $ "[API] Modified video with nr: " ++ show reqNr
      processJsonBody VideoEdit.decoder $ \video ->
        case VideoEdit.toVideo settings video of
          Right videoWithoutIndex -> DB.repsertWithIndex vidsDB (videoWithoutIndex reqNr) Video.nr $> success
          Left error -> Logger.info log error $> makeResponse Unauthorized error

    PostCokk2021Login -> do
      Logger.info log "[API] Login attempt"
      processJsonBody Cokk2021Login.decode $ \login -> do
        users <- DB.everythingList cokk2021UserDB
        let userOpt = List.find (Cokk2021Login.matchesLogin login) users
        maybe
          (return $ makeResponse Unauthorized "Bad Credentials")
          (\user -> do
            waterLogs <- DB.everythingList cokk2021WaterDB
            let dashboardData = Cokk2021DashboardData.make user waterLogs
            return $ makeResponse OK $ Cokk2021DashboardData.encode dashboardData
          )
          userOpt

    PostCokk2021Register -> do
      Logger.info log "[API] Registration attempt"
      processJsonBody Cokk2021Registration.decode $ \registration -> do
        let user = Cokk2021Registration.toUser registration
        users <- DB.everythingList cokk2021UserDB
        if Cokk2021User.username user `elem` map Cokk2021User.username users
        then return $ makeResponse BadRequest "Felhasznalonev nem egyedi."
        else
          if Cokk2021User.eggname user `elem` map Cokk2021User.eggname users
          then return $ makeResponse BadRequest "Tojasnev nem egyedi."
          else do
            DB.insert cokk2021UserDB user
            let dashboardData = Cokk2021DashboardData.make user []
            return $ makeResponse OK $ Cokk2021DashboardData.encode dashboardData

    PostCokk2021Water -> do
      Logger.info log "[API] Watering!"
      processJsonBody Cokk2021WaterRequest.decode $ \req ->
        if Cokk2021WaterRequest.source req == Cokk2021WaterRequest.target req
        then return $ makeResponse BadRequest "Nem ontozheted meg magad! >:|"
        else do
          result <- DB.modifyData cokk2021UserDB $ \users ->
            let
              targetUserOpt = List.find (\u -> Cokk2021WaterRequest.target req == Cokk2021User.username u) users
              sourceUserOpt = List.find (\u -> Cokk2021WaterRequest.source req == Cokk2021User.username u
                                            && Cokk2021WaterRequest.sourcePass req == Cokk2021User.passwordHash u) users
              sourceUserNotFound = (users, Just "Bocs, de rossz a jelszo/felhasznalo")
              targetUserNotFound = (users, Just "Bocs de nem letezik az akit meg akarsz ontozni")
            in case sourceUserOpt of
                Nothing -> sourceUserNotFound
                Just _ ->
                  case targetUserOpt of
                    Nothing -> targetUserNotFound
                    Just targetUser ->
                      let newData =
                            Utils.mapIf
                              (\u -> Cokk2021User.username u == Cokk2021WaterRequest.target req)
                              (Cokk2021User.addPerfume 1)
                              users
                      in (newData, Nothing)

          maybe (do
                  wLog <- Cokk2021WaterLog.make (Cokk2021WaterRequest.source req) (Cokk2021WaterRequest.target req)
                  DB.insert cokk2021WaterDB wLog
                  return $ makeResponse OK "Success"
                )
                (return . makeResponse BadRequest)
                result

    PostCokk2021DashboardRefresh -> do
      Logger.info log "[API] refreshing dashboard"
      processJsonBody Cokk2021Login.decode $ \login -> do
        users <- DB.everythingList cokk2021UserDB
        let userOpt = List.find (Cokk2021Login.matchesLogin login) users
        maybe
          (return $ makeResponse Unauthorized "Bad Credentials")
          (\user -> do
            waterLogs <- DB.everythingList cokk2021WaterDB
            let dashboardData = Cokk2021DashboardData.make user waterLogs
            return $ makeResponse OK $ Cokk2021DashboardData.encode dashboardData
          )
          userOpt

    PostCokk2021IncSkill -> do
      Logger.info log "[API] increase skill"
      processJsonBody Cokk2021IncSkillRequest.decode $ \req -> do
        users <- DB.everythingList cokk2021UserDB
        let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021IncSkillRequest.toLogin req) users
        maybe
          (return $ makeResponse Unauthorized "Bad Credentials")
          (\user -> do
            let skillOpt = Cokk2021Skills.parse $ Cokk2021IncSkillRequest.skill req
            maybe
              (return $ makeResponse BadRequest "No such skill exists!")
              (\skill -> do
                let level = skill $ Cokk2021User.skills user
                if level >= 10
                then return $ makeResponse Forbidden "You can't increase skill level above 10!"
                else
                  if Cokk2021User.perfume user < level + 1
                  then return $ makeResponse PaymentRequired "Not enough perfume!"
                  else do
                    let updatedUser = user
                          { Cokk2021User.perfume = Cokk2021User.perfume user - (level + 1)
                          , Cokk2021User.skills = Maybe.fromMaybe (Cokk2021User.skills user)
                                                $ Cokk2021Skills.incSkill (Cokk2021IncSkillRequest.skill req) (Cokk2021User.skills user)
                          }
                    DB.modifyData cokk2021UserDB $ \users ->
                      ( Utils.mapIf (\u -> Cokk2021User.username u == Cokk2021User.username user) (const updatedUser) users
                      , ()
                      )
                    return $ makeResponse OK "OK Boomer"
              )
              skillOpt
          )
          userOpt

    PostCokk2021ChangeEggname -> do
      Logger.info log "[API] change egg name"
      processJsonBody Cokk2021ChangeEggnameRequest.decode $ \req -> do
        users <- DB.everythingList cokk2021UserDB
        let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021ChangeEggnameRequest.toLogin req) users
        maybe
          (return $ makeResponse Unauthorized "Bad Credentials")
          (\user -> do
            let eggs = map Cokk2021User.eggname users
            let newEggname = Cokk2021ChangeEggnameRequest.newEggname req
            if newEggname `elem` eggs
            then return $ makeResponse Forbidden "Egg already exists!"
            else do
              DB.modifyData cokk2021UserDB
                $ (, ()) . Utils.mapIf
                  (\u -> Cokk2021User.username u == Cokk2021User.username user)
                  (\u -> u { Cokk2021User.eggname = Cokk2021ChangeEggnameRequest.newEggname req })
              return $ makeResponse OK "Ok Boomer"
          )
          userOpt

    PostCokk2021BuyItem -> do
      Logger.info log "[API] buy request"
      processJsonBody Cokk2021BuyItemRequest.decode $ \req -> do
        users <- DB.everythingList cokk2021UserDB
        let userOpt = List.find (Cokk2021Login.matchesLogin $ Cokk2021BuyItemRequest.toLogin req) users
        maybe
          (return $ makeResponse Unauthorized "Bad Credentials")
          (\user -> do
            items <- DB.everythingList cokk2021ItemDB
            let itemOpt = List.find (\i -> Cokk2021Item.index i == Cokk2021BuyItemRequest.index req) items
            maybe
              (return $ makeResponse BadRequest "This item does not exist!")
              (\item -> do
                if Cokk2021Item.index item `elem` Cokk2021User.items user
                then return $ makeResponse BadRequest "Item is already owned!"
                else if Cokk2021User.perfume user < Cokk2021Item.cost item
                then return $ makeResponse PaymentRequired "Not enough perfume"
                else do
                  DB.modifyData cokk2021UserDB
                    $ (, ()) . Utils.mapIf
                      (\u -> Cokk2021User.username u == Cokk2021User.username user)
                      (\u -> u { Cokk2021User.perfume = Cokk2021User.perfume user - Cokk2021Item.cost item
                               , Cokk2021User.items = Cokk2021Item.index item : Cokk2021User.items user
                               , Cokk2021User.base = item
                               }
                      )
                  return $ makeResponse OK "Ok Boomer"
              )
              itemOpt
          )
          userOpt

    DeleteVideoJSON reqNr -> do
      Logger.info log $ "[API] Delete video nr: " ++ show reqNr
      processJsonBody Password.decoder $ \(Password pwd) ->
        if pwd == (settings & password)
        then DB.delete vidsDB (\v -> Video.nr v == reqNr) $> success
        else Logger.info log ("Bad password: " ++ pwd) $> makeResponse Unauthorized ""

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
  Utils.safeReadBinaryFile path
  & fmap (maybe (makeResponse InternalServerError "Could not read file!") (makeResponse OK))
