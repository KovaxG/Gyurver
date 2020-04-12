{-# LANGUAGE NamedFieldPuns #-}
module Events.Cokkolo where

import Data.List (intersperse)
import System.Random

data Tojas = Tojas
  { nev :: String
  , hatterSzin :: String
  } deriving (Show, Read)

piroska = Tojas "Piroska" "red"
sargacska = Tojas "Sargacska" "yellow"

data Harc = Harc
  { tojas1 :: String
  , tojas2 :: String
  , gyoztes :: String
  } deriving (Show)

harc :: (Tojas, Tojas) -> IO Harc
harc (t1, t2) = do
  coin <- randomIO
  let nyertes = Harc (nev t1) (nev t2) . nev
  return $ if coin then nyertes t1 else nyertes t2

tojasokToJson :: [Tojas] -> String
tojasokToJson = surround . concat . intersperse "," . map tojasToJson
  where
    surround :: String -> String
    surround s = "[" ++ s ++ "]"

tojasToJson :: Tojas -> String
tojasToJson Tojas{nev, hatterSzin} =
  "{ \"nev\":\"" ++ nev ++ "\", \"szin\":\"" ++ hatterSzin ++ "\"}"