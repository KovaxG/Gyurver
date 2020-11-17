module Endpoints (Endpoint(..), Resource(..), parseEndpoint, parseResource) where

import           Text.Parsec (ParsecT, Stream, (<|>))
import qualified Text.Parsec as Parsec

import Utils (fromRight, eitherToMaybe, ($>))

data Resource = Resource String String

parseResource :: String -> Maybe Resource
parseResource = eitherToMaybe . Parsec.parse rule "Parsing Resource"
  where
    rule = do
      name <- Parsec.many1 (Parsec.alphaNum <|> Parsec.char '_')
      Parsec.char '.'
      term <- Parsec.many1 Parsec.alphaNum
      return $ Resource name term

data Endpoint
  = GetLandingPage
  | GetCV
  | GetFavicon
  | GetArticlesPage
  | GetCokkJSON
  | GetVideosPage
  | GetVideosJSON
  | GetVideoJSON Int
  | GetCokkResultsPage
  | GetCokkPage
  | GetVideosAddPage
  | GetResource String
  | PostVideo
  | PostVideoJSON Int
  | DeleteVideoJSON Int
  | OptionsVideo
  | OptionsVideoJSON Int
  | Other String
  deriving (Eq, Show)


parseEndpoint :: String -> Endpoint
parseEndpoint s = fromRight (Other s) $ Parsec.parse rule "Parsing Endpoint" s
  where
    rule =
      foldl (<|>) (limit landingPage)
      $ limit <$>
        [ cv, favicon, articlesPageHU, articlesPageRO, articlesPageEN, cokkJson,  videosPageEN, videosJsonHU, cokkResultsPageEN, cokkPage,
          videoAddPageEN, getResource, postVideoEndpointEN, optionsVideoEndpointEN, cokkResultsPageRO, cokkResultsPageHU, videosPageHU, videosPageRO,
          videoAddPageRO, videoAddPageHU, videosJsonRO, videosJsonEN, postVideoEndpointRO, postVideoEndpointHU, optionsVideoEndpointHU, optionsVideoEndpointRO,
          getVideoJson, postVideoJson, optionsVideoJson, deleteVideoJSON
        ]

    landingPage = Parsec.string "GET /" $> GetLandingPage

    articlesPageEN = Parsec.string "GET /articles" $> GetArticlesPage
    articlesPageHU = Parsec.string "GET /cikkek" $> GetArticlesPage
    articlesPageRO = Parsec.string "GET /articole" $> GetArticlesPage

    videosPageEN = Parsec.string "GET /videos" $> GetVideosPage
    videosPageHU = Parsec.string "GET /videok" $> GetVideosPage
    videosPageRO = Parsec.string "GET /videouri" $> GetVideosPage

    videoAddPageEN = Parsec.string "GET /videos/new" $> GetVideosAddPage
    videoAddPageHU = Parsec.string "GET /videok/uj" $> GetVideosAddPage
    videoAddPageRO = Parsec.string "GET /videouri/nou" $> GetVideosAddPage

    videosJsonEN = Parsec.string "GET /api/videos" $> GetVideosJSON
    videosJsonHU = Parsec.string "GET /api/videok" $> GetVideosJSON
    videosJsonRO = Parsec.string "GET /api/videouri" $> GetVideosJSON

    getVideoJson = Parsec.string "GET /api/video/" >> GetVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

    postVideoJson = Parsec.string "POST /api/video/" >> PostVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

    optionsVideoJson = Parsec.string "OPTIONS /api/video/" >> OptionsVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

    deleteVideoJSON = Parsec.string "DELETE /api/video/" >> DeleteVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

    postVideoEndpointEN = Parsec.string "POST /api/videos" $> PostVideo
    postVideoEndpointHU = Parsec.string "POST /api/videok" $> PostVideo
    postVideoEndpointRO = Parsec.string "POST /api/videouri" $> PostVideo

    optionsVideoEndpointEN = Parsec.string "OPTIONS /api/videos" $> OptionsVideo
    optionsVideoEndpointHU = Parsec.string "OPTIONS /api/videok" $> OptionsVideo
    optionsVideoEndpointRO = Parsec.string "OPTIONS /api/videouri" $> OptionsVideo

    cokkPage = Parsec.string "GET /cokk" $> GetCokkPage

    cokkResultsPageEN = Parsec.string "GET /cokk/results" $> GetCokkResultsPage
    cokkResultsPageHU = Parsec.string "GET /cokk/eredmenyek" $> GetCokkResultsPage
    cokkResultsPageRO = Parsec.string "GET /cokk/rezultate" $> GetCokkResultsPage

    cokkJson = Parsec.string "GET /api/cokk" $> GetCokkJSON

    cv = Parsec.string "GET /cv" $> GetCV
    favicon = Parsec.string "GET /favicon.ico" $> GetFavicon
    getResource = Parsec.string "GET /res/" >> GetResource <$> Parsec.many1 validResourceChar

    validResourceChar = Parsec.alphaNum <|> Parsec.char '.' <|> Parsec.char '_'

limit :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
limit rule = Parsec.try (rule <* Parsec.eof)
