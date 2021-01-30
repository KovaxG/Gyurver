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
  | GetCokk2020JSON
  | PostCokk2021Login
  | PostCokk2021Register
  | GetVideosPage
  | GetVideosJSON
  | GetVideoJSON Int
  | GetCokk2020ResultsPage
  | GetCokk2020Page
  | GetCokk2021Page
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
    landingPage = Parsec.string "GET /" $> GetLandingPage

    rule =
      foldl (<|>) (limit landingPage)
      $ limit <$>
        [ landingPage

        , Parsec.string "GET /articles" $> GetArticlesPage
        , Parsec.string "GET /cikkek" $> GetArticlesPage
        , Parsec.string "GET /articole" $> GetArticlesPage

        , Parsec.string "GET /videos" $> GetVideosPage
        , Parsec.string "GET /videok" $> GetVideosPage
        , Parsec.string "GET /videouri" $> GetVideosPage

        , Parsec.string "GET /videos/new" $> GetVideosAddPage
        , Parsec.string "GET /videok/uj" $> GetVideosAddPage
        , Parsec.string "GET /videouri/nou" $> GetVideosAddPage

        , Parsec.string "GET /api/videos" $> GetVideosJSON
        , Parsec.string "GET /api/videok" $> GetVideosJSON
        , Parsec.string "GET /api/videouri" $> GetVideosJSON

        , Parsec.string "GET /api/video/" >> GetVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

        , Parsec.string "POST /api/video/" >> PostVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

        , Parsec.string "OPTIONS /api/video/" >> OptionsVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

        , Parsec.string "DELETE /api/video/" >> DeleteVideoJSON <$> (read <$> Parsec.many1 Parsec.digit)

        , Parsec.string "POST /api/videos" $> PostVideo
        , Parsec.string "POST /api/videok" $> PostVideo
        , Parsec.string "POST /api/videouri" $> PostVideo

        , Parsec.string "OPTIONS /api/videos" $> OptionsVideo
        , Parsec.string "OPTIONS /api/videok" $> OptionsVideo
        , Parsec.string "OPTIONS /api/videouri" $> OptionsVideo

        , Parsec.string "GET /cokk2020" $> GetCokk2020Page
        , Parsec.string "GET /cokk2021" $> GetCokk2021Page

        , Parsec.string "GET /cokk2020/results" $> GetCokk2020ResultsPage
        , Parsec.string "GET /cokk2020/eredmenyek" $> GetCokk2020ResultsPage
        , Parsec.string "GET /cokk2020/rezultate" $> GetCokk2020ResultsPage

        , Parsec.string "GET /api/cokk2020" $> GetCokk2020JSON

        , Parsec.string "POST /api/cokk2021/register" $> PostCokk2021Register
        , Parsec.string "POST /api/cokk2021/login" $> PostCokk2021Login

        , Parsec.string "GET /cv" $> GetCV
        , Parsec.string "GET /favicon.ico" $> GetFavicon

        , Parsec.string "GET /res/" >> GetResource <$> Parsec.many1 validResourceChar
        ]

    validResourceChar = Parsec.alphaNum <|> Parsec.char '.' <|> Parsec.char '_'

limit :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
limit rule = Parsec.try (rule <* Parsec.eof)
