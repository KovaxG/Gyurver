module Events.Cokk2021.Bajnoksag where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import           Events.Cokk2021.Skills (Skills)
import qualified Events.Cokk2021.Skills as Skills
import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Gyurver.Logger (Logger)
import qualified Gyurver.Logger as Logger
import qualified System.Random as Random
import           Utils ((+:))
import qualified Utils

type Nev = String
type HP = Int
type DMG = Int

data Tojas = Tojas
  { nev :: Nev
  , skills :: Skills
  } deriving (Eq, Ord)

instance Show Tojas where
  show = nev

mkTojas :: String -> Skills -> Tojas
mkTojas = Tojas

data Result = Result Nev Nev [Log] deriving (Show)

logs :: Result -> [Log]
logs (Result _ _ ls) = ls

data State = State (Tojas, HP) (Tojas, HP) [Log] deriving (Show)

data Effect
  = ZsirossagCheck
  | BajCheck
  | RegeneracioCheck
  | HumorCheck
  | FurfangossagCheck
  | MuveszlelekCheck
  | SettenkedesCheck
  | MeggyozoeroCheck
  | DiplomaciaCheck
  | TuzokadasCheck
  deriving (Show)

data Log
  = StartFight (Nev, HP) (Nev, HP)
  | Win Nev
  | Damage (Nev, DMG, HP, [Effect]) (Nev, DMG, HP, [Effect])
  | Effect Effect
  deriving (Show)

fight :: (Tojas, Tojas) -> Logger -> IO Result
fight ts logger = prefight ts >>= Utils.finiteIterateM (`fightLoop` logger)

prefight :: (Tojas, Tojas) -> IO State
prefight tt = do
  priorityA <- Random.randomRIO (0,100)
  priorityB <- Random.randomRIO (0,100)
  let (a, b) = Bifunctor.bimap initialHealth initialHealth
             $ if priorityA + Skills.edzettseg (skills $ fst tt) * 5 > priorityB + Skills.edzettseg (skills $ snd tt) * 5
               then tt else Tuple.swap tt
  return $ State a b [StartFight (Bifunctor.first nev a) (Bifunctor.first nev b)]

initialHealth :: Tojas -> (Tojas, HP)
initialHealth t = (t, max 1 (startingHP + kemenysegBonus - szivarozasBonus + vernyomasBonus))
  where
    startingHP = 40
    kemenysegBonus = Skills.kemenyseg (skills t) * 5
    szivarozasBonus = Skills.szivarozas (skills t) * 3
    vernyomasBonus =
      let v = Skills.vernyomas (skills t)
      in if v > 5 then - (v - 5) * 2 else 0

fightLoop :: State -> Logger -> IO (Either Result State)
fightLoop (State (ta, hpa) (tb, hpb) log) logger
  | hpa <= 0 && hpb <= 0 && hpa > hpb  = gameOver ta tb log
  | hpa <= 0 && hpb <= 0 && hpa < hpb  = gameOver tb ta log
  | hpa <= 0 && hpb <= 0 && hpa == hpb = return $ Left undefined
  | hpa <= 0 && hpb > 0 = gameOver tb ta log
  | hpa > 0 && hpb <= 0 = gameOver ta tb log
  | hpa > 0 && hpb > 0 = do

    bajCheckB <- probability BajCheck (Skills.baj (skills tb) + Skills.izles (skills ta) - Skills.intelligencia (skills ta)) (Skills.szerencse (skills tb))

    case bajCheckB of
      Just bc -> do
        let newLog = Damage (nev ta, 0, hpa, []) (nev tb, 0, hpb, Maybe.catMaybes [bajCheckB])
        return $ Right $ State (tb, hpb) (ta, hpa) (log +: newLog)

      Nothing -> do
        zsirossagCheckA <- probability ZsirossagCheck ((Skills.zsirossag (skills ta) - Skills.tisztasagmania (skills ta) - Skills.precizitas (skills tb)) * 5) (Skills.szerencse (skills ta))
        let dmgA = if Maybe.isJust zsirossagCheckA then 0 else Skills.hegyesseg (skills tb)

        settenkedesCheckA <- probability SettenkedesCheck (Skills.settenkedes (skills ta) * 5) (Skills.szerencse (skills ta))
        muveszlelekCheckA <- probability MuveszlelekCheck (Skills.muveszlelek (skills ta) * 2) (- Skills.szerencse (skills ta))
        tuzokadasCheckA <- probability TuzokadasCheck (Skills.tuzokadas (skills ta) * 2) (Skills.szerencse (skills ta))
        zsirossagCheckB <- probability ZsirossagCheck ((Skills.zsirossag (skills tb) - Skills.tisztasagmania (skills tb) - Skills.precizitas (skills ta)) * 5) (Skills.szerencse (skills tb))
        let damage = (if Maybe.isJust settenkedesCheckA then 2 else 1) * (if Maybe.isJust muveszlelekCheckA then 0 else 3)
        let tuzDamage = if Maybe.isJust tuzokadasCheckA then 30 else 0
        let dmgB = if Maybe.isJust zsirossagCheckB then 0 else damage + Skills.erosseg (skills ta) + tuzDamage

        let hpA = hpa - dmgA

        regeneracioCheckB <- probability RegeneracioCheck (Skills.regeneracio (skills tb) * 5) (Skills.szerencse (skills tb))
        let preRegenHP = hpb - dmgB
        let maxHPB = snd $ initialHealth tb
        let hpB = min maxHPB (preRegenHP + if Maybe.isJust regeneracioCheckB then 3 else 0)

        humorCheckB <- probability HumorCheck (Skills.humorerzek (skills tb)) (Skills.szerencse (skills tb))

        furfangossagB <- probability FurfangossagCheck (Skills.furfangossag (skills tb) * 5) (Skills.szerencse (skills tb))

        let (hpAFinal, hpBFinal, furfangossagB2) =
              if Maybe.isJust furfangossagB && hpB < hpA
              then (hpB, hpA, furfangossagB)
              else (hpA, hpB, Nothing)

        let newLog =
              Damage (nev ta, dmgA, hpAFinal, Maybe.catMaybes [zsirossagCheckA, muveszlelekCheckA, tuzokadasCheckA])
                     (nev tb, dmgB, hpBFinal, Maybe.catMaybes [zsirossagCheckB, regeneracioCheckB, humorCheckB, furfangossagB2, settenkedesCheckA])
        let newState = State (tb, hpBFinal) (ta, hpAFinal) (log +: newLog)
        --Logger.debug logger newState
        return $ Right newState

gameOver :: Tojas -> Tojas -> [Log] -> IO (Either Result State)
gameOver winner looser log = do
  diplomaciaCheckLooser <- probability DiplomaciaCheck (Skills.diplomacia (skills looser) * 5) (Skills.szerencse (skills looser))
  meggyozoeroCheckLooser <- probability MeggyozoeroCheck (Skills.meggyozoero (skills looser) * 2) 0
  maybe
    (maybe
      (return $ Left $ Result (nev winner) (nev looser) (log +: Win (nev winner)))
      (\megcheck -> return $ Left $ Result (nev looser) (nev winner) (log +: Effect megcheck +: Win (nev looser)))
      meggyozoeroCheckLooser
    )
    (\restart -> do
      (State (ta, hpa) (tb, hpb) nlog) <- prefight (winner, looser)
      return $ Right $ State (ta, hpa) (tb, hpb) (log +: Effect restart)
    ) diplomaciaCheckLooser

probability :: a -> Int -> Int -> IO (Maybe a)
probability a p pp = do
  n <- Random.randomRIO (0, 100)
  return $ if p + (pp * 2) > n then Just a else Nothing

encodeLog :: Log -> Json
encodeLog log = case log of
  Win name -> JsonObject [("type", JsonString "win"), ("winner", JsonString name)]
  Effect eff -> JsonObject [("type", JsonString "eff"), ("effect", JsonString $ show eff)]
  StartFight (na, hpa) (nb, hpb) ->
    JsonObject
      [ ("type", JsonString "start")
      , ("nameA", JsonString na), ("hpA", JsonNumber $ fromIntegral hpa)
      , ("nameB", JsonString nb), ("hpB", JsonNumber $ fromIntegral hpb)
      ]
  Damage (na, dmgA, hpA, effsA) (nb, dmgB, hpB, effsB) ->
    JsonObject
      [ ("type", JsonString "damage")
      , ("nameA", JsonString na)
      , ("dmgA", JsonNumber $ fromIntegral dmgA)
      , ("hpA", JsonNumber $ fromIntegral hpA)
      , ("effsA", JsonArray $ map (JsonString . show) effsA)
      , ("nameB", JsonString nb)
      , ("dmgB", JsonNumber $ fromIntegral dmgB)
      , ("hpB", JsonNumber $ fromIntegral hpB)
      , ("effsB", JsonArray $ map (JsonString . show) effsB)
      ]


{-
  https://scorecounter.com/tournament/

[DEBUG   2021-04-30 23:06:56.630984362 EEST]
State (Tojgli,-18) (Tojika,23)
[ StartFight ("Tojgli",50) ("Tojika",75)
, Damage ("Tojgli",5,45,[]) ("Tojika",0,75,[ZsirossagCheck])
, Damage ("Tojika",0,35,[ZsirossagCheck]) ("Tojgli",10,75,[FurfangossagCheck,SettenkedesCheck])
, Damage ("Tojgli",5,70,[]) ("Tojika",5,30,[])
, Damage ("Tojika",1,29,[TuzokadasCheck]) ("Tojgli",37,33,[])
, Damage ("Tojgli",5,28,[]) ("Tojika",0,29,[ZsirossagCheck,SettenkedesCheck])
, Damage ("Tojika",1,28,[]) ("Tojgli",7,24,[RegeneracioCheck])
, Damage ("Tojgli",5,19,[]) ("Tojika",5,23,[])
, Damage ("Tojika",0,23,[ZsirossagCheck,TuzokadasCheck]) ("Tojgli",37,-18,[])
]


-}
