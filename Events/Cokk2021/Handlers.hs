{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.Handlers where

import qualified Types.DateTime as DateTime
import           Data.Function ((&))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Gyurver.Response (Status(..), Response)
import qualified Gyurver.Response as Response
import qualified Gyurver.Logger as Logger
import           Gyurver.Logger (Logger)
import           Types.Settings (Settings)
import qualified Types.Settings as Settings
import qualified Types.Common as Types
import           Component.Database (DBHandle)
import qualified Component.Database as DB
import           Events.Cokk2021.User (User)
import qualified Events.Cokk2021.User as User
import           Events.Cokk2021.Login (Login)
import qualified Events.Cokk2021.Login as Login
import qualified Events.Cokk2021.Item as Item
import           Events.Cokk2021.Item (Item)
import qualified Events.Cokk2021.DashboardData as Dashboard
import           Events.Cokk2021.WaterLog (WaterLog)
import qualified Events.Cokk2021.WaterLog as WaterLog
import qualified Events.Cokk2021.WaterRequest as WaterRequest
import qualified Events.Cokk2021.FightRequest as FightRequest
import qualified Events.Cokk2021.Registration as Register
import qualified Events.Cokk2021.IncSkillRequest as IncSkillRequest
import qualified Events.Cokk2021.Skills as Skills
import qualified Utils

getParticipants :: DBHandle User -> DBHandle WaterLog -> IO Response
getParticipants userDB waterDB = do
  users <- DB.everythingList userDB
  waterLogs <- DB.everythingList waterDB
  let usersJson = map (User.toListItemJson waterLogs True) users
  Response.addHeaders [("Content-Type", "application/json")]
    <$> Response.make OK usersJson

getParticipantsForUser :: Text -> DBHandle User -> DBHandle WaterLog -> IO Response
getParticipantsForUser content userDB waterDB =
  Response.processJsonBody content Login.decode $ \login -> do
    users <- DB.everythingList userDB
    users & List.find (Login.matchesLogin login)
          & maybe
            (Response.make Unauthorized ("Bad Credentials" :: Text))
            (\user -> do
              waterLogs <- DB.everythingList waterDB
              now <- DateTime.getCurrentDateTime
              let relevantLines = filter (\w -> WaterLog.wlSource w == User.username user) waterLogs
              let nusers =
                    map (\u ->
                      ( maybe True (flip WaterRequest.isWaterable now . WaterLog.wlDateTime)
                          $ Utils.safeLast
                          $ List.sortOn WaterLog.wlDateTime
                          $ filter (\w -> WaterLog.wlTarget w == User.username u) relevantLines
                      , u
                      )
                    ) users

              Response.addHeaders [("Content-Type", "application/json")]
                <$> Response.make OK (map (uncurry $ User.toListItemJson waterLogs) nusers)
            )

fight :: Text -> DBHandle User -> Logger -> IO Response
fight content userDB log =
  Response.processJsonBody content FightRequest.decode $ \req -> do
    users <- DB.everythingList userDB
    let targetUserOpt = List.find (\u -> FightRequest.target req == User.username u) users
    let sourceUserOpt = List.find (\u -> FightRequest.source req == User.username u
                                        && FightRequest.sourcePass req == User.passwordHash u) users
    case sourceUserOpt of
      Nothing -> Response.make Unauthorized ("Bocs, de rossz a jelszo/felhasznalo" :: Text)
      Just sourceUser ->
        case targetUserOpt of
          Nothing -> Response.make BadRequest ("Bocs de nem letezik az akit meg akarsz ontozni" :: Text)
          Just targetUser -> do
            fightResult <- FightRequest.runFight sourceUser targetUser log
            Response.make OK fightResult

getItems :: DBHandle Item -> IO Response
getItems itemDB = do
  items <- DB.everythingList itemDB
  Response.make OK $ map Item.encode items

login :: Text -> DBHandle User -> DBHandle WaterLog -> Logger -> IO Response
login content userDB waterDB log = do
  Logger.info log $ "[API] Login attempt with body " <> content
  Response.processJsonBody content Login.decode $ \login -> do
    users <- DB.everythingList userDB
    users
        & List.find (Login.matchesLogin login)
        & maybe
          (do
            Logger.warn log $ "User \"" <> Login.user login <> "\" not found!"
            Response.make Unauthorized ("Bad Credentials" :: Text)
          )
          (\user -> do
            waterLogs <- DB.everythingList waterDB
            let dashboardData = Dashboard.make user waterLogs
            Logger.info log $ "Login success for \"" <> Login.user login <> "\"."
            Response.make OK $ Dashboard.encode dashboardData
          )

register :: Text -> DBHandle User -> Settings -> Logger -> IO Response
register content userDB settings log = do
  Logger.info log $ "[API] Registration attempt with body: " <> content
  if (settings & Settings.cokk2021) == Types.Running
  then
    Response.processJsonBody content Register.decode $ \registration -> do
      let user = Register.toUser registration
      users <- DB.everythingList userDB
      if User.username user `elem` map User.username users
      then Response.make BadRequest ("Felhasznalonev nem egyedi." :: Text)
      else
        if User.eggname user `elem` map User.eggname users
        then Response.make BadRequest ("Tojasnev nem egyedi." :: Text)
        else do
          DB.insert userDB user
          let dashboardData = Dashboard.make user []
          Response.make OK $ Dashboard.encode dashboardData
  else do
    Logger.info log "[API] The event is not running anymore! "
    Response.make Forbidden ("The event is not running anymore!" :: Text)

water :: Text -> DBHandle User -> DBHandle WaterLog -> Logger -> Settings -> IO Response
water content userDB waterDB log settings = do
  Logger.info log $ "[API] Watering with body: " <> content

  if (settings & Settings.cokk2021) == Types.Blocked
  then do
    Logger.info log "Event is locked!"
    Response.make Forbidden ("Event is locked!" :: Text)
  else do
    Response.processJsonBody content WaterRequest.decode $ \req ->
      if WaterRequest.source req == WaterRequest.target req
      then Response.make BadRequest ("Nem ontozheted meg magad! >:|" :: Text)
      else do
        wLogs <- DB.everythingList waterDB
        wLog <- WaterLog.make (WaterRequest.source req) (WaterRequest.target req)

        let illegal =
              wLogs
              & filter (\l -> WaterLog.wlSource l == WaterLog.wlSource wLog && WaterLog.wlTarget l == WaterLog.wlTarget wLog)
              & map (DateTime.toDate . WaterLog.wlDateTime)
              & any (\d -> d == DateTime.toDate (WaterLog.wlDateTime wLog))

        if illegal
        then Response.make Forbidden ("🖕" :: Text)
        else do
          result <- DB.modifyData userDB $ \users ->
            let
              targetUserOpt = List.find (\u -> WaterRequest.target req == User.username u) users
              sourceUserOpt = List.find (\u -> WaterRequest.source req == User.username u
                                        && WaterRequest.sourcePass req == User.passwordHash u) users
              sourceUserNotFound = (users, Just "Bocs, de rossz a jelszo/felhasznalo") :: ([User], Maybe Text)
              targetUserNotFound = (users, Just "Bocs de nem letezik az akit meg akarsz ontozni")
            in case sourceUserOpt of
              Nothing -> sourceUserNotFound
              Just _ ->
                case targetUserOpt of
                  Nothing -> targetUserNotFound
                  Just targetUser ->
                    let newData =
                          Utils.mapIf
                            (\u -> User.username u == WaterRequest.target req)
                            (User.addPerfume 1)
                            users
                    in (newData, Nothing)

          maybe
            (DB.insert waterDB wLog >> Response.make OK ("Success" :: Text))
            (Response.make BadRequest)
            result

refreshDashboard :: Text -> DBHandle User -> DBHandle WaterLog -> IO Response
refreshDashboard content userDB waterDB =
  Response.processJsonBody content Login.decode $ \login -> do
    users <- DB.everythingList userDB
    users
      & List.find (Login.matchesLogin login)
      & maybe
        (Response.make Unauthorized ("Bad Credentials" :: Text))
        (\user -> do
          waterLogs <- DB.everythingList waterDB
          let dashboardData = Dashboard.make user waterLogs
          Response.make OK $ Dashboard.encode dashboardData
        )

incSkill :: Text -> DBHandle User -> Logger -> Settings -> IO Response
incSkill content userDB log settings = do
  Logger.info log $ "[API] increase skill with body: " <> content

  if (settings & Settings.cokk2021) == Types.Blocked
  then do
    Logger.info log "Event is locked!"
    Response.make Forbidden ("Event is locked!" :: Text)
  else do
    Response.processJsonBody content IncSkillRequest.decode $ \req -> do
      users <- DB.everythingList userDB
      let userOpt = List.find (Login.matchesLogin $ IncSkillRequest.toLogin req) users
      maybe
        (Response.make Unauthorized ("Bad Credentials" :: Text))
        (\user -> do
          let skillOpt = Skills.parse $ IncSkillRequest.skill req
          maybe
            (Response.make BadRequest ("No such skill exists!" :: Text))
            (\skill -> do
              let level = skill $ User.skills user
              if level >= 10
              then Response.make Forbidden ("You can't increase skill level above 10!" :: Text)
              else
                if User.perfume user < level + 1
                then Response.make PaymentRequired ("Not enough perfume!" :: Text)
                else do
                  let updatedUser = user
                        { User.perfume = User.perfume user - (level + 1)
                        , User.skills = Maybe.fromMaybe (User.skills user)
                                              $ Skills.incSkill (IncSkillRequest.skill req) (User.skills user)
                        }
                  DB.modifyData userDB $ \users ->
                    ( Utils.mapIf (\u -> User.username u == User.username user) (const updatedUser) users
                    , ()
                    )
                  Response.make OK ("OK Boomer" :: Text)
            ) skillOpt
        ) userOpt
