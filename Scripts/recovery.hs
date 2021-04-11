module Main where

import qualified Events.Cokk2021.WaterLog as WaterLog
import qualified Component.Decoder as Decoder
import qualified Component.Json as Json
import qualified Data.List as List

main :: IO ()
main = do
  contents <- readFile "Data/cokk2021Water.db"
  let sorok = lines contents
  let logs = map (either undefined id . Decoder.run WaterLog.decode . either undefined id . Json.parseJson) sorok
  putStrLn $ show $ map (\l -> (head l, length l)) $ List.group $ List.sort $ map WaterLog.wlTarget logs

{-
  [ ("Gyuri",99) ok
  , ("Petra",93) ok
  , ("Tamas",95) ok
  , ("Agi",93) ok
  , ("Andor",89) ok
  , ("Brigi",77) ok
  , ("Ceci",84) ok
  , ("Ferko",44) ok
  , ("Hezirisz",48) no pass
  , ("Karola",84) ok
  , ("Kristof",95) ok
  , ("Kriszti",92) ok
  , ("Miklos",43) ok
  , ("RETEK",59) ok
  , ("Rudolf",90) no pass
  , ("Rés",67) ok
  , ("Terebesi",68) ok
  , ("Tms",78) no pass
  , ("Tokmag",60) no pass
  , ("Tünde ",69) ok
  , ("Tünő árnyék",74) ok
  , ("VaranTavers",82) no pass
  , ("Zsolt",90) no pass
  , ("andnor",85) no pass
  , ("bloazs",85) no pass
  , ("kismi19",82) no pass
  , ("Összekoccanunk",85) no pass
  ]
-}
