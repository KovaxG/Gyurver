{-# LANGUAGE OverloadedStrings #-}
module Component.Json where

import           Data.Bifunctor (Bifunctor(..))
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Parsec
import           Text.Printf (printf)

import Utils (($>))

data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString Text
  | JsonArray [Json]
  | JsonObject [(Text, Json)]

nullable :: (a -> Json) -> Maybe a -> Json
nullable = maybe JsonNull

toString :: Json -> Text
toString json = case json of
  JsonNull -> "null"
  (JsonBool True) -> "true"
  (JsonBool False) -> "false"
  -- This was added because JsonNumber 0.0000001 produces stuff like 1e-10 or something :[]
  (JsonNumber num) -> Text.pack $ printf "%f" num
  (JsonString str) -> "\"" <> str <> "\"" -- removed escaping characters
  (JsonArray jsons) -> "[" <> Text.intercalate "," (map toString jsons) <> "]"
  (JsonObject assocList) ->
    "{" <> Text.intercalate "," (map lineToString assocList) <> "}"
    where
      lineToString (label, value) = "\"" <> label <> "\"" <> ":" <> toString value

-- The normal show for strings escapes every single special
-- character and I can't use it in the db because show followed
-- by a read yields different results
-- Ex. show "Jó" --> "J\243"
-- Maybe a better way is to replace escaped characters, but nobody got time for dat
stringify :: String -> String
stringify s = "\"" ++ (escape =<< s) ++ "\""
  where
    escape '\"' = "\\\""
    escape c = [c]

parseJson :: Text -> Either Text Json
parseJson = first (Text.pack . show) . parse json ""
  where
    json =
      jsonNull
      <|> jsonBool
      <|> jsonNumber
      <|> jsonString
      <|> jsonArray
      <|> jsonObject

    jsonNull = string "null" $> JsonNull

    jsonBool = jsonTrue <|> jsonFalse
    jsonTrue = string "true" $> JsonBool True
    jsonFalse = string "false" $> JsonBool False

    jsonNumber = try jsonFloat <|> jsonInt
    jsonInt = do
      number <- read <$> many1 digit
      return $ JsonNumber number
    jsonFloat = do
      number <- many1 digit
      char '.'
      rest <- many1 digit
      return $ JsonNumber $ read $ number ++ "." ++ rest

    jsonString = JsonString <$> stringCharacters

    stringCharacters = do
      char '\"'
      -- TODO not great, can't have quotes in strings :(
      str <- Text.pack <$> many (noneOf "\"")
      char '\"'
      return str

    -- TODO fails at "[ 1 ]" for some reason
    -- the spaces between brackets don't work :(
    jsonArray = do
      char '['
      spaces
      elements <- sepBy json (spaces >> char ',' >> spaces)
      spaces
      char ']'
      return $ JsonArray elements

    jsonObject = do
      char '{'
      spaces
      attrs <- sepBy jsonAttr (spaces >> char ',' >> spaces)
      spaces
      char '}'
      return $ JsonObject $ List.sortOn fst attrs
    jsonAttr = do
      fieldName <- stringCharacters
      spaces
      char ':'
      spaces
      element <- json
      return (fieldName, element)

