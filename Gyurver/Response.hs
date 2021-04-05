{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Gyurver.Response
  ( Response
  , Status(..)
  , toByteString
  , makeResponse
  , addHeaders
  , success
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Time as Time

import Gyurver.Html (Document)
import Component.Json

data Status
  = OK
  | BadRequest
  | Unauthorized
  | InternalServerError
  | PaymentRequired
  | Forbidden

instance Show Status where
  show status =
    case status of
      OK -> "200 OK"
      BadRequest -> "400 Bad Request"
      Unauthorized -> "401 Unauthorized"
      InternalServerError -> "500 Internal Server Error"
      PaymentRequired -> "402 Payment Required"
      Forbidden -> "403 Forbidden"

data Response = Response
  { status  :: Status
  , address :: String
  , headers :: [(String, String)]
  , content :: ByteString
  } deriving (Show)

toByteString :: Response -> ByteString
toByteString Response{content, status, headers} =
  BS.unlines
    [ BS.pack $ "HTTP/1.1 " ++ show status
    , BS.pack $ concatMap showHeader headers
    , content
    ]

addHeaders :: [(String, String)] -> Response -> Response
addHeaders hs r = r { headers = headers r ++ hs }

makeResponse :: CanSend a => Status -> a -> IO Response
makeResponse status content = do
  now <- Time.getCurrentTime
  let payload = toBytes content
  return $ Response
    { status = status
    , address = "localhost"
    , headers =
      [ ("Date", show now)
      , ("Server", "Gyurver") -- TODO maybe add the version here?
      , ("Content-Length", show $ BS.length payload)
      ]
    , content = payload
    }

success :: IO Response
success = makeResponse OK "OK Boomer"

class CanSend a where
  toBytes :: a -> ByteString

instance CanSend ByteString where
  toBytes = id

instance CanSend String where
  toBytes = BS.pack

instance CanSend Document where
  toBytes = BS.pack . show

instance CanSend Json where
  toBytes = BS.pack . show

instance CanSend [Json] where
  toBytes = toBytes . JsonArray

showHeader :: (String, String) -> String
showHeader (k, v) = k ++ ": " ++ v ++ "\n"
