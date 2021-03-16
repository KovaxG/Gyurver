module Events.Cokk2021.Skills where

import qualified Data.Text as Text

import           Component.Json (Json(..))
import qualified Component.Json as Json
import           Component.Database (DBFormat(..))
import           Component.Decoder (Decoder)
import qualified Component.Decoder as Decoder
import qualified Utils

data Skills = Skills
  { kemenyseg :: Int
  , erosseg :: Int
  , settenkedes :: Int
  , szivarozas :: Int
  , furfangossag :: Int
  , tuzokadas :: Int
  , zsirossag :: Int
  , intelligencia :: Int
  , diplomacia :: Int
  , hegyesseg :: Int
  , szerencse :: Int
  , baj :: Int
  , meggyozoero :: Int
  , precizitas :: Int
  , nyelvtudas :: Int
  , izles :: Int
  , vernyomas :: Int
  , humorerzek :: Int
  , regeneracio :: Int
  , muveszlelek :: Int
  , tisztasagmania :: Int
  , edzettseg :: Int
  } deriving (Show, Eq, Ord)

initial :: Skills
initial = Skills 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

encode :: Skills -> Json
encode s = JsonObject
  [ ("kemenyseg", jsonInt kemenyseg)
  , ("erosseg", jsonInt erosseg)
  , ("settenkedes", jsonInt settenkedes)
  , ("szivarozas", jsonInt szivarozas)
  , ("furfangossag", jsonInt furfangossag)
  , ("tuzokadas", jsonInt tuzokadas)
  , ("zsirossag", jsonInt zsirossag)
  , ("intelligencia", jsonInt intelligencia)
  , ("diplomacia", jsonInt diplomacia)
  , ("hegyesseg", jsonInt hegyesseg)
  , ("szerencse", jsonInt szerencse)
  , ("baj", jsonInt baj)
  , ("meggyozoero", jsonInt meggyozoero)
  , ("precizitas", jsonInt precizitas)
  , ("nyelvtudas", jsonInt nyelvtudas)
  , ("izles", jsonInt izles)
  , ("vernyomas", jsonInt vernyomas)
  , ("humorerzek", jsonInt humorerzek)
  , ("regeneracio", jsonInt regeneracio)
  , ("muveszlelek", jsonInt muveszlelek)
  , ("tisztasagmania", jsonInt tisztasagmania)
  , ("edzettseg", jsonInt edzettseg)
  ]
  where jsonInt field = JsonNumber $ fromIntegral $ field s

decode :: Decoder Skills
decode =
  Skills <$> Decoder.field "kemenyseg" Decoder.int
         <*> Decoder.field "erosseg" Decoder.int
         <*> Decoder.field "settenkedes" Decoder.int
         <*> Decoder.field "szivarozas" Decoder.int
         <*> Decoder.field "furfangossag" Decoder.int
         <*> Decoder.field "tuzokadas" Decoder.int
         <*> Decoder.field "zsirossag" Decoder.int
         <*> Decoder.field "intelligencia" Decoder.int
         <*> Decoder.field "diplomacia" Decoder.int
         <*> Decoder.field "hegyesseg" Decoder.int
         <*> Decoder.field "szerencse" Decoder.int
         <*> Decoder.field "baj" Decoder.int
         <*> Decoder.field "meggyozoero" Decoder.int
         <*> Decoder.field "precizitas" Decoder.int
         <*> Decoder.field "nyelvtudas" Decoder.int
         <*> Decoder.field "izles" Decoder.int
         <*> Decoder.field "vernyomas" Decoder.int
         <*> Decoder.field "humorerzek" Decoder.int
         <*> Decoder.field "regeneracio" Decoder.int
         <*> Decoder.field "muveszlelek" Decoder.int
         <*> Decoder.field "tisztasagmania" Decoder.int
         <*> Decoder.field "edzettseg" Decoder.int

instance DBFormat Skills where
  encode = Text.pack . show . Events.Cokk2021.Skills.encode
  decode =
    Utils.eitherToMaybe
    . (=<<) (Decoder.run Events.Cokk2021.Skills.decode)
    . Json.parseJson
    . Text.unpack

parse :: String -> Maybe (Skills -> Int)
parse s = case s of
  "kemenyseg" -> Just kemenyseg
  "erosseg" -> Just erosseg
  "settenkedes" -> Just settenkedes
  "szivarozas" -> Just szivarozas
  "furfangossag" -> Just furfangossag
  "tuzokadas" -> Just tuzokadas
  "zsirossag" -> Just zsirossag
  "intelligencia" -> Just intelligencia
  "diplomacia" -> Just diplomacia
  "hegyesseg" -> Just hegyesseg
  "szerencse" -> Just szerencse
  "baj" -> Just baj
  "meggyozoero" -> Just meggyozoero
  "precizitas" -> Just precizitas
  "nyelvtudas" -> Just nyelvtudas
  "izles" -> Just izles
  "vernyomas" -> Just vernyomas
  "humorerzek" -> Just humorerzek
  "regeneracio" -> Just regeneracio
  "muveszlelek" -> Just muveszlelek
  "tisztasagmania" -> Just tisztasagmania
  "edzettseg" -> Just edzettseg
  _ -> Nothing
