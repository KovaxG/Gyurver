{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gyurver.Response
  ( Response
  , Status(..)
  , toByteString
  , makeResponse
  ) where

import Data.Monoid
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

data Status
  = OK
  | BadRequest

instance Show Status where
  show status =
    case status of
      OK -> "200 OK"
      BadRequest -> "400 Bad Request"

data Response = Response
  { status  :: Status
  , address :: String
  , content :: ByteString
  } deriving (Show)

toByteString :: Response -> ByteString
toByteString Response{content, status} =
  BS.unlines
    [ BS.pack $ "HTTP/1.1 " <> show status
    , BS.pack ""
    , content
    ]

makeResponse :: CanSend a => Status -> a -> Response
makeResponse status content = Response
  { status = status
  , address = "localhost"
  , content = toBytes content
  }

class CanSend a where
  toBytes :: a -> ByteString

instance CanSend ByteString where
  toBytes = id

instance CanSend String where
  toBytes = BS.pack