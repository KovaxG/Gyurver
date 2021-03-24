module Events.Cokk2021.User where

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
import qualified Utils

data User = User
  { username :: String
  , passwordHash :: String
  , eggname :: String
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
  encode = Text.pack . show . Events.Cokk2021.User.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.User.decode)
    . Json.parseJson
    . Text.unpack

addPerfume :: Int -> User -> User
addPerfume p u = u { perfume = perfume u + p }

toListItemJson :: Bool -> User -> Json
toListItemJson b u = JsonObject
  [ ("username", JsonString $ username u)
  , ("eggname", JsonString $ eggname u)
  , ("base", Item.encode $ base u)
  , ("waterable", JsonBool b)
  , ("skills", Skills.encode $ skills u)
  ]
