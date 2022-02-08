{-# LANGUAGE OverloadedStrings #-}

module Types.Rights
  ( Row
  , Right(..)
  , readSecret
  , addSecret
  , AddSecretResponse(..)
  , updateSecret
  , UpdateSecretResponse(..)
  , deleteSecret
  , DeleteSecretResponse(..)
  , rowDecoder
  , toJsonRows
  , getAll
  , allowed
  , movie
  , video
  ) where

import qualified Data.Char as Char
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import           Component.Database (DBHandle, DBFormat(..))
import qualified Component.Database as DB
import           Types.Password (Password(..))
import qualified Utils
import Types.Video (videoToJson)

newtype Secret = Secret Text deriving (Eq)

newtype Right = R Text deriving (Eq, Ord)

instance Show Right where
  show (R s) = Text.unpack s

movie :: Right
movie = R "Movie"

video :: Right
video = R "Video"

data Row = Row
  { enabled :: Bool
  , secret :: Secret
  , rights :: Set Right
  }

showSecret :: Secret -> Text
showSecret (Secret s) = s

readSecret :: Text -> Maybe Secret
readSecret txt =
  case Text.words $ Text.map (\c -> if c == '@' then ' ' else c) txt of
    [user, key] ->
      if Text.length user >= 3 && Text.length key >= 4 && Text.all (\c -> Char.isAlphaNum c || c == '@') (user <> key)
      then Just $ Secret $ user <> "@" <> key
      else Nothing
    _ -> Nothing

showRight :: Right -> Text
showRight = Text.pack . show

readRight :: Text -> Right
readRight = R

toJsonRows :: [Row] -> Json
toJsonRows = JsonArray . fmap toJsonRow

toJsonRow :: Row -> Json
toJsonRow row = JsonObject
  [ ("enabled", JsonBool $ enabled row)
  , ("secret", JsonString $ showSecret $ secret row)
  , ("rights", JsonArray $ fmap (JsonString . showRight) $ Set.toList $ rights row)
  ]

allowed :: DBHandle Row -> Maybe Text -> Right -> IO a -> IO a -> IO a
allowed _ Nothing _ _ sad = sad
allowed rightsDB (Just pass) right happy sad = do
  rows <- DB.everythingList rightsDB
  let maybeRow = List.find (\(Row enabled (Secret secret) _) -> secret == pass && enabled) rows
  let isHappy = maybe False (\(Row _ _ rs) -> right `elem` rs) maybeRow
  if isHappy then happy else sad

showEnabled :: Bool -> Text
showEnabled b = if b then "+" else "-"

readEnabled :: Text -> Maybe Bool
readEnabled t = case t of
  "+" -> Just True
  "-" -> Just False
  _ -> Nothing

instance DBFormat Row where
  encode (Row e s rs) = showEnabled e <> " " <> showSecret s <> " " <> Text.unwords (showRight <$> Set.toList rs)
  decode txt = case Text.words txt of
    (e:s:rs) -> Row <$> readEnabled e <*> readSecret s <*> Just (Set.fromList (map readRight rs))
    _ -> Nothing

rowDecoder :: Decoder Row
rowDecoder =
  Row
    <$> Decoder.field "enabled" Decoder.bool
    <*> Decoder.field "secret" secretDecoder
    <*> Decoder.field "rights" rightsDecoder
  where
    secretDecoder :: Decoder Secret
    secretDecoder = Decoder.withParser readSecret "Failed to decode secret."

    rightDecoder :: Decoder Right
    rightDecoder = fmap R Decoder.string

    rightsDecoder :: Decoder (Set Right)
    rightsDecoder = Set.fromList <$> Decoder.list rightDecoder

data AddSecretResponse = AddedSuccessfully | SecretExists

addSecret :: DBHandle Row -> Row -> IO AddSecretResponse
addSecret rowDB newRow = do
  let newSecret = secret newRow
  rows <- DB.everythingList rowDB
  if newSecret `elem` (secret <$> rows)
  then return SecretExists
  else do
    DB.insert rowDB newRow
    return AddedSuccessfully

getAll :: DBHandle Row -> IO [Row]
getAll = DB.everythingList

data UpdateSecretResponse = UpdatedSuccessfuly | SecretNotExists

updateSecret :: DBHandle Row -> Row -> IO UpdateSecretResponse
updateSecret rowDB row =
  DB.modifyData rowDB $ \rows ->
    if secret row `elem` (secret <$> rows)
    then (Utils.mapIf (\r -> secret r == secret row) (const row) rows, UpdatedSuccessfuly)
    else (rows, SecretNotExists)

data DeleteSecretResponse = DeletedSuccessfuly | SecretNotFound | InvalidSecret

deleteSecret :: DBHandle Row -> Text -> IO DeleteSecretResponse
deleteSecret rowDB sec =
  maybe (return InvalidSecret)
        (\sec ->
          DB.modifyData rowDB $ \rows ->
            if sec `elem` (secret <$> rows)
            then (filter (\r -> secret r /= sec) rows, DeletedSuccessfuly)
            else (rows, SecretNotFound)
        )
        (readSecret sec)
