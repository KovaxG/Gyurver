{-# LANGUAGE NamedFieldPuns #-}
module Events.Cokkolo where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Component.Database (DBFormat(..))
import           Component.Json (Json(..))

data Tojas = Tojas
  { nev :: Text
  , hatterSzin :: Text
  } deriving (Show)

piroska :: Tojas
piroska = Tojas (Text.pack "Piroska") (Text.pack "red")

sargacska :: Tojas
sargacska = Tojas (Text.pack "Sargacska") (Text.pack "yellow")

instance DBFormat Tojas where
  encode Tojas{nev, hatterSzin} =
    Text.intercalate (Text.pack ", ") [nev, hatterSzin]
  decode s =
    let (szinR, nevR) = Text.span (/=',') $ Text.reverse s
        hatterSzin = Text.reverse $ Text.strip szinR
        nev = Text.reverse $ Text.tail nevR
    in Just $ Tojas { nev, hatterSzin }

tojasToJson :: Tojas -> Json
tojasToJson Tojas{nev, hatterSzin} =
  JsonObject
    [ ("nev", JsonString (Text.unpack nev))
    , ("szin", JsonString (Text.unpack hatterSzin))
    ]
