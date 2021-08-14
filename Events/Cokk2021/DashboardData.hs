{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.DashboardData where

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Events.Cokk2021.User (User)
import qualified Events.Cokk2021.User as User
import           Events.Cokk2021.WaterLog (WaterLog)
import qualified Events.Cokk2021.WaterLog as WaterLog


data DashboardData = DashboardData
  { userData :: User
  , events :: [WaterLog]
  }

make :: User -> [WaterLog] -> DashboardData
make user log =
  let outgoing = filter (\l -> WaterLog.wlSource l == User.username user) log
      incoming = filter (\l -> WaterLog.wlTarget l == User.username user) log
  in DashboardData user (outgoing ++ incoming)

encode :: DashboardData -> Json
encode (DashboardData user events) = JsonObject
  [ ("user", User.encode user)
  , ("events", JsonArray $ WaterLog.encode <$> events)
  ]
