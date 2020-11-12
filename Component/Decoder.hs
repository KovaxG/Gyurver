{-# LANGUAGE LambdaCase #-}

module Component.Decoder (
  Decoder(..), int, double, bool, string, list, maybe, field, success, failure
) where

import Prelude hiding (maybe)
import Data.List (lookup)

import Component.Json

newtype Decoder a = Decoder { run :: Json -> Either String a }

instance Functor Decoder where
  fmap f decoder = Decoder $ \json -> fmap f (run decoder json)

instance Applicative Decoder where
  pure = success
  decoderf <*> decoder = Decoder $ \json -> run decoderf json <*> run decoder json

int :: Decoder Int
int = Decoder $ \case
    JsonNumber num ->
      if fromIntegral (round num) == num then Right (round num)
      else Left $ "Expected int, but got float, " ++ show num
    other -> Left $ "Could not decode int, was given " ++ show other

double :: Decoder Double
double = Decoder $ \case
    JsonNumber num -> Right num
    other -> Left $ "Could not decode double, was given " ++ show other

bool :: Decoder Bool
bool = Decoder $ \case
  JsonBool b -> Right b
  other -> Left $ "Could not decode bool, was given " ++ show other

string :: Decoder String
string = Decoder $ \case
  JsonString str -> Right str
  other -> Left $ "Could not decode string, was given " ++ show other

list :: Decoder a -> Decoder [a]
list itemDecoder = Decoder $ \case
  JsonArray array -> traverse (run itemDecoder) array
  other -> Left $ "Could not decode array, was given " ++ show other

maybe :: Decoder a -> Decoder (Maybe a)
maybe itemDecoder = Decoder $ \case
  JsonNull -> Right Nothing
  other -> Just <$> run itemDecoder other

field :: String -> Decoder a -> Decoder a
field name itemDecoder =
  Decoder $ \case
    JsonObject obj -> case lookup name obj of
      Just item -> run itemDecoder item
      Nothing -> Left $ "Field " ++ name ++ " is not present in object."

    other -> Left $ "Could not decode object field " ++ name ++ ", was given " ++ show other

success :: a -> Decoder a
success a = Decoder $ \_ -> Right a

failure :: String -> Decoder a
failure message = Decoder $ \_ -> Left message
