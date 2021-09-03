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

import           Gyurver.Html (Document)
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
  }

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
make status thing = do
  now <- Time.getCurrentTime
  let response = toBytes thing
  return $ response
    { status = status
    , address = "localhost"
    , headers = headers response ++
      [ ("Date", show now)
      , ("Server", "Gyurver") -- TODO maybe add the version here?
      , ("Content-Length", show $ BS.length $ content response)
      , ("Connection", "close")
      ]
    }

success :: IO Response
success = make OK ("OK Boomer" :: Text)

class CanSend a where
  toBytes :: a -> Response

instance CanSend () where
  toBytes () = Response
    { status = OK
    , address = "localhost"
    , headers = []
    , content = BS.empty
    }

instance CanSend ByteString where
  toBytes b = Response
    { status = OK
    , address = "localhost"
    , headers = [] -- ByteString could be anything!
    , content = b
    }

instance CanSend Text where
  toBytes t = Response
    { status = OK
    , address = "localhost"
    , headers = [] -- Text could be anything!
    , content = TextEncoding.encodeUtf8 t
    }

instance CanSend Document where
  toBytes d = Response
    { status = OK
    , address = "localhost"
    , headers = [("Content-Type", "text/html")]
    , content = BS.pack $ show d
    }

instance CanSend Json where
  toBytes j = Response
    { status = OK
    , address = "localhost"
    , headers = [("Content-Type", "application/json")]
    , content = TextEncoding.encodeUtf8 $ Json.toString j
    }

instance CanSend [Json] where
  toBytes = toBytes . Json.JsonArray

showHeader :: (String, String) -> String
showHeader (k, v) = k ++ ": " ++ v ++ "\n"
