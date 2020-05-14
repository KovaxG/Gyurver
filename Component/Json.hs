module Component.Json where

import Data.Bifunctor
import Data.List (intersperse)
import Text.Parsec

import Utils

data Json 
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double 
  | JsonString String 
  | JsonArray [Json] 
  | JsonObject [(String, Json)]

nullable :: (a -> Json) -> Maybe a -> Json
nullable f ma = maybe JsonNull f ma
  
instance Show Json where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber num) = show num
  show (JsonString str) = show str 
  show (JsonArray jsons) = show jsons
  show (JsonObject assocList) = 
    "{" ++ (concat $ intersperse "," $ map lineToString assocList) ++ "}"
    where
      lineToString (label, value) = 
        show label ++ ":" ++ show value

parseJson :: String -> Either String Json
parseJson = first show . parse json ""
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
      
    jsonString = do
      str <- stringCharacters
      return $ JsonString str
    
    stringCharacters = do
      char '\"'
      -- TODO not great, can't have quotes in strings :(
      str <- many (noneOf "\"") 
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
      return $ JsonObject attrs
    jsonAttr = do
      fieldName <- stringCharacters
      spaces
      char ':'
      spaces
      element <- json
      return (fieldName, element)
      
