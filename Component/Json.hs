module Component.Json where

import Data.List (intersperse)

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
