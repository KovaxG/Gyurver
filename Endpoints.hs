module Endpoints
  ( Endpoint(..)
  , Resource(..)
  , Operation(..)
  , RightsOperation(..)
  , parse
  , parseResource
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Parsec (ParsecT, Stream, (<|>))
import qualified Text.Parsec as Parsec

import Utils (fromRight, eitherToMaybe, ($>))

data Resource = Resource Text Text

data Operation
  = Insert
  | Modify
  | Obtain
  | Delete
  deriving (Show, Eq)

data RightsOperation
  = GetAll
  | AddSecret
  | UpdateSecret
  | DeleteSecret
  deriving (Show, Eq)

parseResource :: Text -> Maybe Resource
parseResource = eitherToMaybe . Parsec.parse rule "Parsing Resource"
  where
    rule = do
      name <- Text.pack <$> Parsec.many1 (Parsec.alphaNum <|> Parsec.char '_')
      Parsec.char '.'
      term <- Text.pack <$> Parsec.many1 Parsec.alphaNum
      return $ Resource name term

data Endpoint
  = Ping
  | GetLandingPage
  | GetCV
  | GetFavicon
  | GetArticlesPage
  | GetCokk2020JSON
  | PostCokk2021Login
  | PostCokk2021Register
  | GetCokk2021Participants
  | PostCokk2021ParticipantsForUser
  | PostCokk2021DashboardRefresh
  | PostCokk2021Water
  | PostCokk2021IncSkill
  | PostCokk2021ChangeEggname
  | GetCokk2021Items
  | PostCokk2021BuyItem
  | PostCokk2021EquipItem
  | PostCokk2021Fight
  | PostSuggestion
  | GetVideosPage
  | GetVideosJSON
  | GetVideoJSON Int
  | GetCokk2020ResultsPage
  | GetCokk2021ResultsPage
  | GetCokk2020Page
  | GetCokk2021Page
  | GetVideosAddPage
  | GetResource Text
  | PostVideo
  | PostVideoJSON Int
  | DeleteVideoJSON Int
  | OptionsVideo
  | OptionsVideoJSON Int
  | Film Operation
  | GetFilmsPage
  | GetBlogPage
  | GetBlogJSON Int
  | GetBlogItemsJSON
  | GetBlogItemPage Int
  | Other Text
  | Rights RightsOperation
  | RightsPage
  deriving (Eq, Show)


parse :: Text -> Endpoint
parse s = fromRight (Other s) $ Parsec.parse rule "Parsing Endpoint" s
  where
    landingPage = Parsec.string "GET /" $> GetLandingPage

    rule =
      foldl (<|>) (limit landingPage)
      $ limit <$>
        [ Parsec.string "GET /api/ping" $> Ping

        , landingPage

        , Parsec.string "GET /articles" $> GetArticlesPage
        , Parsec.string "GET /cikkek" $> GetArticlesPage
        , Parsec.string "GET /articole" $> GetArticlesPage

        , Parsec.string "GET /videos" $> GetVideosPage
        , Parsec.string "GET /videok" $> GetVideosPage
        , Parsec.string "GET /videouri" $> GetVideosPage

        , Parsec.string "GET /blog" $> GetBlogPage
        , Parsec.string "GET /blog/" >> GetBlogItemPage <$> (read <$> Parsec.many1 Parsec.digit)

        , Parsec.string "GET /api/blog/items" $> GetBlogItemsJSON
        , Parsec.string "GET /api/blog/" >> GetBlogJSON <$> (read <$> Parsec.many1 Parsec.digit)

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
        , Parsec.string "GET /cokk2021/results" $> GetCokk2021ResultsPage

        , Parsec.string "GET /api/cokk2020" $> GetCokk2020JSON

        , Parsec.string "POST /api/cokk2021/register" $> PostCokk2021Register
        , Parsec.string "POST /api/cokk2021/login" $> PostCokk2021Login
        , Parsec.string "GET /api/cokk2021/resztvevok" $> GetCokk2021Participants
        , Parsec.string "POST /api/cokk2021/resztvevok" $> PostCokk2021ParticipantsForUser
        , Parsec.string "POST /api/cokk2021/water" $> PostCokk2021Water
        , Parsec.string "POST /api/cokk2021/dashboard" $> PostCokk2021DashboardRefresh
        , Parsec.string "POST /api/cokk2021/skills/inc" $> PostCokk2021IncSkill
        , Parsec.string "POST /api/cokk2021/update/eggname" $> PostCokk2021ChangeEggname
        , Parsec.string "GET /api/cokk2021/items" $> GetCokk2021Items
        , Parsec.string "POST /api/cokk2021/items/buy" $> PostCokk2021BuyItem
        , Parsec.string "POST /api/cokk2021/items/equip" $> PostCokk2021EquipItem
        , Parsec.string "POST /api/cokk2021/fight" $> PostCokk2021Fight

        , Parsec.string "POST /api/films" $> Film Insert
        , Parsec.string "PUT /api/films" $> Film Modify
        , Parsec.string "GET /api/films" $> Film Obtain
        , Parsec.string "DELETE /api/films" $> Film Delete
        , Parsec.string "GET /films" $> GetFilmsPage

        , Parsec.string "GET /api/rights" $> Rights GetAll
        , Parsec.string "POST /api/rights" $> Rights AddSecret
        , Parsec.string "PUT /api/rights" $> Rights UpdateSecret
        , Parsec.string "DELETE /api/rights" $> Rights DeleteSecret

        , Parsec.string "GET /jogok" $> RightsPage

        , Parsec.string "POST /api/suggestionbox" $> PostSuggestion

        , Parsec.string "GET /cv" $> GetCV
        , Parsec.string "GET /favicon.ico" $> GetFavicon

        , Parsec.string "GET /res/" >> GetResource . Text.pack <$> Parsec.many1 validResourceChar
        ]

    validResourceChar = Parsec.alphaNum <|> Parsec.char '.' <|> Parsec.char '_'

limit :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
limit rule = Parsec.try (rule <* Parsec.eof)
