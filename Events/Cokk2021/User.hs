{-# LANGUAGE OverloadedStrings #-}

module Events.Cokk2021.User where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import qualified Component.Database as DB
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Events.Cokk2021.Skills (Skills)
import qualified Events.Cokk2021.Skills as Skills
import           Events.Cokk2021.Item (Item)
import qualified Events.Cokk2021.Item as Item
import           Events.Cokk2021.WaterLog (WaterLog)
import qualified Events.Cokk2021.WaterLog as WaterLog
import qualified Utils

data User = User
  { username :: Text
  , passwordHash :: Text
  , eggname :: Text
  , perfume :: Int
  , base :: Item
  , skills :: Skills
  , items :: [Int]
  }

encode :: User -> Json
encode user = JsonObject
  [ ("username", JsonString $ username user)
  , ("password", JsonString $ passwordHash user)
  , ("eggname", JsonString $ eggname user)
  , ("perfume", JsonNumber $ fromIntegral $ perfume user)
  , ("base", Item.encode $ base user)
  , ("skills", Skills.encode $ skills user)
  , ("items", JsonArray $ map (JsonNumber . fromIntegral) $ items user)
  ]

decode :: Decoder User
decode =
  User <$> Decoder.field "username" Decoder.string
       <*> Decoder.field "password" Decoder.string
       <*> Decoder.field "eggname" Decoder.string
       <*> Decoder.field "perfume" Decoder.int
       <*> Decoder.field "base" Item.decode
       <*> Decoder.field "skills" Skills.decode
       <*> Decoder.field "items" (Decoder.list Decoder.int)

instance DBFormat User where
  encode = Json.toString . Events.Cokk2021.User.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.User.decode)
    . Json.parseJson

addPerfume :: Int -> User -> User
addPerfume p u = u { perfume = perfume u + p }

toListItemJson :: [WaterLog] -> Bool -> User -> Json
toListItemJson wlogs b u = JsonObject
  [ ("username", JsonString $ username u)
  , ("eggname", JsonString $ eggname u)
  , ("base", Item.encode $ base u)
  , ("waterable", JsonBool b)
  , ("skills", Skills.encode $ skills u)
  , ("ontozott", JsonNumber $ fromIntegral ontozott)
  , ("ontoztek", JsonNumber $ fromIntegral ontoztek)
  ]
  where
    ontozott = Utils.count (\wl -> WaterLog.wlSource wl == username u) wlogs
    ontoztek = Utils.count (\wl -> WaterLog.wlTarget wl == username u) wlogs
