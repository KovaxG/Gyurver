module Types.PageHit (PageHit, make) where

import           Component.Database (DBFormat(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Types.DateTime (DateTime)
import qualified Types.DateTime as DateTime

data PageHit = PageHit
  { date :: DateTime
  , method :: Text
  , url :: Text
  } deriving (Show, Eq)

make :: DateTime -> Text -> Text -> PageHit
make d m u = PageHit d m u

textEncode :: PageHit -> Text
textEncode (PageHit dt m u) = Text.unwords [DateTime.textEncoder dt, m, u]

textDecode :: Text -> Maybe PageHit
textDecode txt = case Text.words txt of
  [datet, method, url] -> do
    date <- DateTime.textDecoder datet
    return $ PageHit date method url
  _ -> Nothing

instance DBFormat PageHit where
  encode = textEncode
  decode = textDecode
