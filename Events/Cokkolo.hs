{-# LANGUAGE NamedFieldPuns #-}
module Events.Cokkolo where

import Data.List (intersperse)
import System.Random

data Szin = Piros | Zold | Sarga deriving (Show, Read)
data Motivum = Semmi deriving (Show, Read)
data Strategia = A | V deriving (Show, Read)

data Tojas = Tojas
  { nev :: String
  , hatterSzin :: Szin
  , motivum :: Motivum
  } deriving (Show, Read)

piroska = Tojas "Piroska" Piros Semmi
sargacska = Tojas "Sargacska" Sarga Semmi

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
  "{ \"nev\":\"" ++ nev ++ "\", \"szin\":\"" ++ showSzin hatterSzin ++ "\"}"
  where
    showSzin :: Szin -> String
    showSzin szin =
      case szin of
        Piros -> "red"
        Zold -> "green"
        Sarga -> "yellow"