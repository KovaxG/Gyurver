module Events.Cokkolo where

data Szin = Piros | Zold | Sarga deriving (Show, Read)
data Motivum = Semmi deriving (Show, Read)
data Strategia = A | V deriving (Show, Read)

data Tojas = Tojas
  { nev :: String
  , hatterSzin :: Szin
  , motivum :: Motivum
  , strategiak :: [Strategia]
  } deriving (Show, Read)

piroska = Tojas "Piroska" Piros Semmi [A, V, A] 
sargacska = Tojas "Sargacska" Sarga Semmi [V, A, A] 

data Harc = Harc
  { tojas1 :: String
  , tojas2 :: String
  , gyoztes :: String
  } deriving (Show)

harc :: (Tojas, Tojas) -> Harc
harc (t1, t2) = 
  let 
    nyertes = Harc (nev t1) (nev t2)
    (hp1, hp2) = 
      vegigCokkoles (3,3) 
      $ map csapas 
      $ zip (cycle $ strategiak t1) 
            (cycle $ strategiak t2)
  in
    if hp1 >= hp2
    then nyertes (nev t1)
    else nyertes (nev t2)
  where
    csapas :: (Strategia, Strategia) -> (Int, Int)
    csapas (V, A) = (-1, -1)
    csapas (A, V) = (-1, -1)
    csapas (A, A) = (-1,  0)
    csapas (V, V) = ( 0, -1)

    vegigCokkoles :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
    vegigCokkoles (hp1, hp2) [] = (hp1, hp2)
    vegigCokkoles (hp1, hp2) ((diff1, diff2):stb)
      | hp1 == 0 || hp2 == 0 = (hp1, hp2)
      | otherwise = vegigCokkoles (hp1 + diff1, hp2 + diff2) stb

-- TODO add MVar for blocking
-- Maybe extract into a DB module with read and write?
filebaMentes :: String -> [Tojas] -> IO ()
filebaMentes path tojasok = 
  writeFile path (show tojasok)

filebolOlvasas :: String -> IO [Tojas]
filebolOlvasas path =
  read <$> readFile path