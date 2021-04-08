module Events.Cokk2021.Bajnoksag where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import           Events.Cokk2021.Skills (Skills)
import qualified Events.Cokk2021.Skills as Skills
import qualified System.Random as Random
import           Utils ((+:))
import qualified Utils

type Nev = String
type HP = Int
type DMG = Int

data Tojas = Tojas
  { nev :: Nev
  , skills :: Skills
  } deriving (Show, Eq, Ord)

mkTojas :: String -> Tojas
mkTojas name = Tojas name Skills.initial

data Result = Result Nev Nev [Log] deriving (Show)
data State = State (Tojas, HP) (Tojas, HP) [Log] deriving (Show)

data Effect
  = ZsirossagCheck
  | BajCheck
  | RegeneracioCheck
  | HumorCheck
  | FurfangossagCheck
  | MuveszlelekCheck
  | SettenkedesCheck
  deriving (Show)

data Log
  = StartFight (Nev, HP) (Nev, HP)
  | Win Nev
  | Damage (Nev, DMG, HP, [Effect]) (Nev, DMG, HP, [Effect])
  deriving (Show)

fight :: (Tojas, Tojas) -> IO Result
fight ts = prefight ts >>= Utils.finiteIterateM fightLoop

prefight :: (Tojas, Tojas) -> IO State
prefight tt = do
  priorityA <- Random.randomIO :: IO Int
  priorityB <- Random.randomIO :: IO Int
  let (a, b) = Bifunctor.bimap initialHealth initialHealth $ if priorityA > priorityB then tt else Tuple.swap tt
  return $ State a b [StartFight (Bifunctor.first nev a) (Bifunctor.first nev b)]

initialHealth :: Tojas -> (Tojas, Int)
initialHealth t = (t, max 1 (startingHP + kemenysegBonus - szivarozasBonus))
  where
    startingHP = 40
    kemenysegBonus = Skills.kemenyseg (skills t) * 5
    szivarozasBonus = Skills.szivarozas (skills t) * 3

fightLoop :: State -> IO (Either Result State)
fightLoop (State (ta, hpa) (tb, hpb) log)
  | hpa <= 0 && hpb <= 0 && hpa > hpb  = return $ Left $ Result (nev ta) (nev tb) (log +: Win (nev ta))
  | hpa <= 0 && hpb <= 0 && hpa < hpb  = return $ Left $ Result (nev tb) (nev ta) (log +: Win (nev tb))
  | hpa <= 0 && hpb <= 0 && hpa == hpb = return $ Left undefined
  | hpa <= 0 && hpb > 0 = return $ Left $ Result (nev tb) (nev ta) (log +: Win (nev tb))
  | hpa > 0 && hpb <= 0 = return $ Left $ Result (nev ta) (nev tb) (log +: Win (nev ta))
  | hpa > 0 && hpb > 0 = do

    bajCheckB <- probability BajCheck (Skills.baj (skills tb) + Skills.izles (skills ta)) (Skills.szerencse (skills tb))

    zsirossagCheckA <- probability ZsirossagCheck ((Skills.zsirossag (skills ta) - Skills.tisztasagmania (skills ta)) * 5) (Skills.szerencse (skills ta))
    let dmgA = if Maybe.isJust zsirossagCheckA then 0 else Skills.hegyesseg (skills tb)

    settenkedesCheckA <- probability SettenkedesCheck (Skills.settenkedes (skills ta) * 5) (Skills.szerencse (skills ta))
    muveszlelekCheckA <- probability MuveszlelekCheck (Skills.muveszlelek (skills ta) * 2) (- Skills.szerencse (skills ta))
    zsirossagCheckB <- probability ZsirossagCheck ((Skills.zsirossag (skills tb) - Skills.tisztasagmania (skills tb)) * 5) (Skills.szerencse (skills tb))
    let damage = (if Maybe.isJust settenkedesCheckA then 2 else 1) * (if Maybe.isJust muveszlelekCheckA then 0 else 3)
    let dmgB = if Maybe.isJust zsirossagCheckB then 0 else damage + Skills.erosseg (skills ta)

    let hpA = hpa - dmgA

    regeneracioCheckB <- probability RegeneracioCheck (Skills.regeneracio (skills tb) * 5) (Skills.szerencse (skills tb))
    let hpB = hpb - dmgB + if Maybe.isJust regeneracioCheckB then 5 else 0

    humorCheckB <- probability HumorCheck (Skills.humorerzek (skills tb)) (Skills.szerencse (skills tb))

    furfangossagB <- probability FurfangossagCheck (Skills.furfangossag (skills tb) * 5) (Skills.szerencse (skills tb))

    let (hpAF, hpBF) = if Maybe.isJust furfangossagB && hpB < hpA then (hpB, hpA) else (hpA, hpB)

    let newLog =
          if Maybe.isJust bajCheckB
          then Damage (nev ta, 0, hpa, Maybe.catMaybes [bajCheckB])
                      (nev tb, 0, hpb, [])
          else Damage (nev ta, dmgA, hpAF, Maybe.catMaybes [zsirossagCheckA, muveszlelekCheckA])
                      (nev tb, dmgB, hpBF, Maybe.catMaybes [zsirossagCheckB, regeneracioCheckB, humorCheckB, furfangossagB, settenkedesCheckA])

    return $ Right $ State (tb, hpB) (ta, hpA) (log +: newLog)

probability :: a -> Int -> Int -> IO (Maybe a)
probability a p pp = do
  n <- Random.randomRIO (0, 100)
  return $ if p + (pp * 5) > n then Just a else Nothing
