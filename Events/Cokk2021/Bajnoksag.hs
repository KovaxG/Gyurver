module Events.Cokk2021.Bajnoksag where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import           Events.Cokk2021.Skills (Skills)
import qualified Events.Cokk2021.Skills as Skills
import           Utils ((+:))

type Nev = String

data Tojas = Tojas
  { nev :: Nev
  , skills :: Skills
  } deriving (Show, Eq, Ord)

mkTojas :: String -> Tojas
mkTojas name = Tojas name Skills.initial

data MeccsEredmeny = ME (Nev, Int) (Nev, Int) [String] deriving (Show)

versenyzok = [(\t -> t { skills = (skills t) {Skills.erosseg = 1} }) $ mkTojas "tojgli", (\t -> t { skills = (skills t) { Skills.kemenyseg = 1 } }) $ mkTojas "gulu", mkTojas "mojgli"]

fuss :: [Tojas] -> (Map Nev Int, [MeccsEredmeny])
fuss versenyzok = (vegsoEredmenyek, meccsEredmenyek)
  where
    meccsek = filter (uncurry (/=)) $ map (\[a, b] -> (a, b)) $ List.nub $ map List.sort $ sequence [versenyzok, versenyzok]
    meccsEredmenyek = map jatek meccsek
    vegsoEredmenyek = Map.fromListWith (+) $ meccsEredmenyek >>= eredmenyek

    eredmenyek :: MeccsEredmeny -> [(Nev, Int)]
    eredmenyek (ME ae be _) = [ae, be]

jatek :: (Tojas, Tojas) -> MeccsEredmeny
jatek (a, b) =
  let startingHP = 40
      ahp = startingHP + Skills.kemenyseg (skills a)
      bhp = startingHP + Skills.kemenyseg (skills b)
  in fight (a, ahp) (b, bhp) ["Elkezdodott a meccs " ++ nev a ++ "(" ++ show ahp ++ ") vs " ++ nev b ++ "(" ++ show bhp ++ ")"]

fight :: (Tojas, Int) -> (Tojas, Int) -> [String] -> MeccsEredmeny
fight (a, hpa) (b, hpb) story
  | hpa <= 0 && hpb <= 0 = ME (nev a, 1) (nev b, 1) (story +: "egyenlo")
  | hpa <= 0 = ME (nev a, 0) (nev b, 3) (story +: nev b ++ " nyert!")
  | hpb <= 0 = ME (nev a, 3) (nev b, 0) (story +: nev a ++ " nyert!")
  | otherwise =
    let defaultDMG = 3
        dmg = defaultDMG + Skills.erosseg (skills a)
    in fight (b, hpb - dmg)
             (a, hpa)
             (story +: nev a ++ "(" ++ show hpa ++ ") tamad, " ++ nev b ++ " " ++ show dmg ++ " pontot sebzodik")
