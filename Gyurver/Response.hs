{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gyurver.Response
  ( Response
  , Status(..)
  , toByteString
  , makeResponse
  , addHeaders
  , success
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Gyurver.Html (Document)

data Status
  = OK
  | BadRequest
  | Unauthorized
  | InternalServerError

instance Show Status where
  show status =
    case status of
      OK -> "200 OK"
      BadRequest -> "400 Bad Request"
      Unauthorized -> "401 Unauthorized"
      InternalServerError -> "500 Internal Server Error"

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

makeResponse :: CanSend a => Status -> a -> Response
makeResponse status content = Response
  { status = status
  , address = "localhost"
  , headers = []
  , content = toBytes content
  }

success :: Response
success = makeResponse OK ""

class CanSend a where
  toBytes :: a -> ByteString

instance CanSend ByteString where
  toBytes = id

instance CanSend String where
  toBytes = BS.pack

instance CanSend Document where
  toBytes = BS.pack . show

showHeader :: (String, String) -> String
showHeader (k, v) = k ++ ": " ++ v ++ "\n"
