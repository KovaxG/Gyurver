module Endpoints where

import           Text.Parsec (ParsecT, Stream, (<|>))
import qualified Text.Parsec as Parsec

import Utils (fromRight, ($>))

data Endpoint
  = GetLandingPage
  | GetCV
  | GetFavicon
  | GetArticlesPage
  | GetCokkJSON
  | GetVideosPage
  | GetVideosJSON
  | GetCokkResultsPage
  | GetCokkPage
  | GetVideosAddPage
  | GetResource String
  | PostVideo
  | OptionsVideo
  | Other String
  deriving (Eq, Show)


parseEndpoint :: String -> Endpoint
parseEndpoint s = fromRight (Other s) $ Parsec.parse rule "Parsing Endpoint" s
  where
    rule = foldl (<|>) (limit landingPage)
         $ fmap limit
         $ [ cv, favicon, articlesPage, cokkJson, videosPage, videosJson, cokkResultsPage, cokkPage,
             videoAddPage, getResource, postVideoEndpoint, optionsVideoEndpoint
           ]

    landingPage = Parsec.string "GET /" $> GetLandingPage
    cv = Parsec.string "GET /cv" $> GetCV
    favicon = Parsec.string "GET /favicon.ico" $> GetFavicon
    articlesPage = Parsec.string "GET /articles" $> GetArticlesPage
    cokkJson = Parsec.string "GET /cokk/list" $> GetCokkJSON
    videosPage = Parsec.string "GET /vids" $> GetVideosPage
    videosJson = Parsec.string "GET /api/vids" $> GetVideosJSON
    cokkResultsPage = Parsec.string "GET /cokk/eredmeny" $> GetCokkResultsPage
    cokkPage = Parsec.string "GET /cokk" $> GetCokkPage
    videoAddPage = Parsec.string "GET /vids/add" $> GetVideosAddPage
    getResource = Parsec.string "GET /res/" >> GetResource <$> Parsec.many1 (Parsec.alphaNum <|> Parsec.char '.' <|> Parsec.char '_')
    postVideoEndpoint = Parsec.string "POST /api/vids" $> PostVideo
    optionsVideoEndpoint = Parsec.string "OPTIONS /api/vids" $> OptionsVideo



limit :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
limit rule = Parsec.try (rule <* Parsec.eof)
