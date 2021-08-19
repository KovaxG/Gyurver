{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Component.Decoder (
  Decoder(..), int, double, bool, string, list, maybe, field, success, failure
) where

import           Prelude hiding (maybe)
import           Data.List (lookup)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))

import           Component.Json (Json(..))
import qualified Component.Json as Json

newtype Decoder a = Decoder { run :: Json -> Either Text a }

instance Functor Decoder where
  fmap f decoder = Decoder $ \json -> fmap f (run decoder json)

instance Applicative Decoder where
  pure = success
  decoderf <*> decoder = Decoder $ \json -> run decoderf json <*> run decoder json

-- run da json, lol
instance Monad Decoder where
  da >>= df = Decoder $ \json -> run da json >>= (\a -> run (df a) json)

int :: Decoder Int
int = Decoder $ \case
    JsonNumber num ->
      if fromIntegral (round num) == num then Right (round num)
      else Left $ "Expected int, but got float, " <> Text.pack (show num)
    other -> Left $ "Could not decode int, was given " <> Json.toString other

double :: Decoder Double
double = Decoder $ \case
    JsonNumber num -> Right num
    other -> Left $ "Could not decode double, was given " <> Json.toString other

bool :: Decoder Bool
bool = Decoder $ \case
  JsonBool b -> Right b
  other -> Left $ "Could not decode bool, was given " <> Json.toString other

string :: Decoder Text
string = Decoder $ \case
  JsonString str -> Right str
  other -> Left $ "Could not decode string, was given " <> Json.toString other

list :: Decoder a -> Decoder [a]
list itemDecoder = Decoder $ \case
  JsonArray array -> traverse (run itemDecoder) array
  other -> Left $ "Could not decode array, was given " <> Json.toString other

maybe :: Decoder a -> Decoder (Maybe a)
maybe itemDecoder = Decoder $ \case
  JsonNull -> Right Nothing
  other -> Just <$> run itemDecoder other

field :: Text -> Decoder a -> Decoder a
field name itemDecoder =
  Decoder $ \case
    JsonObject obj -> case lookup name obj of
      Just item -> run itemDecoder item
      Nothing -> Left $ "Field " <> name <> " is not present in object."

    other -> Left $ "Could not decode object field " <> name <> ", was given " <> Json.toString other

success :: a -> Decoder a
success a = Decoder $ \_ -> Right a

failure :: Text -> Decoder a
failure message = Decoder $ \_ -> Left message
