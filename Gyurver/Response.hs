{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Gyurver.Response
  ( Response
  , Status(..)
  , toByteString
  , make
  , addHeaders
  , success
  , processJsonBody
  ) where

import           Data.Function ((&))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Time as Time
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import Gyurver.Html (Document)
import qualified Component.Json as Json
import           Component.Json (Json)
import qualified Component.Decoder as Decoder

data Status
  = OK
  | BadRequest
  | Unauthorized
  | NotFound
  | InternalServerError
  | PaymentRequired
  | Forbidden

instance Show Status where
  show status =
    case status of
      OK -> "200 OK"
      BadRequest -> "400 Bad Request"
      Unauthorized -> "401 Unauthorized"
      NotFound -> "404 Not Found"
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

processJsonBody :: Text -> Decoder.Decoder a -> (a -> IO Response) -> IO Response
processJsonBody content decoder handle =
  Json.parseJson content >>= Decoder.run decoder & either (make BadRequest) handle

addHeaders :: [(String, String)] -> Response -> Response
addHeaders hs r = r { headers = headers r ++ hs }

make :: CanSend a => Status -> a -> IO Response
make status content = do
  now <- Time.getCurrentTime
  let payload = toBytes content
  return $ Response
    { status = status
    , address = "localhost"
    , headers =
      [ ("Date", show now)
      , ("Server", "Gyurver") -- TODO maybe add the version here?
      , ("Content-Length", show $ BS.length payload)
      , ("Connection", "close")
      ]
    , content = payload
    }

success :: IO Response
success = make OK ("OK Boomer" :: Text)

class CanSend a where
  toBytes :: a -> ByteString

instance CanSend ByteString where
  toBytes = id

instance CanSend Text where
  toBytes = TextEncoding.encodeUtf8

instance CanSend Document where
  toBytes = BS.pack . show

instance CanSend Json where
  toBytes = TextEncoding.encodeUtf8 . Json.toString

instance CanSend [Json] where
  toBytes = toBytes . Json.JsonArray

showHeader :: (String, String) -> String
showHeader (k, v) = k ++ ": " ++ v ++ "\n"
