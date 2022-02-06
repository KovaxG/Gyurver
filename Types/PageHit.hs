module Types.PageHit (PageHit, make, date) where

import           Component.Database (DBFormat(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Types.DateTime (DateTime)
import qualified Types.DateTime as DateTime
import qualified Utils

data PageHit = PageHit
  { date :: DateTime
  , method :: Text
  , url :: Text
  , millis :: Double
  } deriving (Show, Eq)

make :: DateTime -> Text -> Text -> Double -> PageHit
make = PageHit

textEncode :: PageHit -> Text
textEncode (PageHit dt m u ms) = Text.unwords [DateTime.textEncoder dt, m, u, Text.pack $ show ms]

textDecode :: Text -> Maybe PageHit
textDecode txt = case Text.words txt of
  [datet, method, url, mst] -> do
    date <- DateTime.textDecoder datet
    ms <- Utils.safeRead mst
    return $ PageHit date method url ms
  _ -> Nothing

instance DBFormat PageHit where
  encode = textEncode
  decode = textDecode
