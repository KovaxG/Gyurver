{-# LANGUAGE NamedFieldPuns #-}
module Gyurver.Response 
  ( Response
  , Status(..)
  , toByteString
  , mkResponse
  ) where

import Data.ByteString.Char8 (ByteString, pack)

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
  , content :: String
  } deriving (Show)

toByteString :: Response -> ByteString
toByteString Response{content, status} =
  pack $  unlines
    [ "HTTP/1.1 " ++ show status
    , ""
    , content
    ]

mkResponse :: Status -> String -> Response
mkResponse status content = Response
  { status = status
  , address = "localhost"
  , content = content
  }