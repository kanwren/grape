{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language ImportQualifiedPost #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Main (main) where

import Data.Char (isUpper, toLower)
import Data.Fixed (Centi)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString (ByteString)

import Control.Concurrent.Async (forConcurrently)
import Control.Lens
import Data.Aeson qualified as Aeson
import Network.HTTP.Req ((=:), (/:))
import Network.HTTP.Req qualified as Req
import Options.Applicative
import Text.Xml.Lens qualified as X

httpCfg :: Req.HttpConfig
httpCfg = Req.defaultHttpConfig

baseURI :: Req.Url 'Req.Https
baseURI = Req.https "www.webscraper.io" /: "test-sites" /: "e-commerce" /: "static" /: "computers" /: "laptops"

getPage :: Int -> IO (Either String ByteString)
getPage p = Req.runReq httpCfg $ do
  r <- Req.req Req.GET baseURI Req.NoReqBody Req.bsResponse $ "page" =: p
  let statusCode = Req.responseStatusCode r
  if statusCode == 200
    then pure $ Right $ Req.responseBody r
    else pure $ Left $ "Unexpected error code: " <> show statusCode

data Laptop = Laptop
  { laptopName :: !Text
  , laptopPrice :: !Centi
  , laptopDescription :: !Text
  , laptopStars :: !Int
  }
  deriving stock (Show, Generic)

toSnake :: String -> String
toSnake s = s & _head %~ toLower & _tail %~ snakeRest
  where
    snakeRest "" = ""
    snakeRest (c:cs)
      | isUpper c = '_' : toLower c : snakeRest cs
      | otherwise = c : snakeRest cs

instance Aeson.ToJSON Laptop where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \f -> toSnake $ fromMaybe f $ stripPrefix "laptop" f
    }

scrapePage :: ByteString -> Maybe [Laptop]
scrapePage page = do
  let cosmosAt p = cosmosOf (filtered (not . p) . plate) . filtered p
      findWithClass className =
        let classes = X.elementAttributes . ix "class" . folding Text.words
        in cosmosAt (elemOf classes className)
  let laptops = page ^.. lazy . X.html . X.node "body" . findWithClass "thumbnail"

  for laptops $ \l -> do
    c <- l ^? findWithClass "caption"
    name <- c ^? findWithClass "title" . X.attr "title" . traverse
    description <- c ^? findWithClass "description" . X.text
    price <- c ^? findWithClass "price" . X.text
      . to (readMaybe @Centi . drop 1 . Text.unpack) . traverse
    stars <- l ^? cosmos . X.attr "data-rating" . traverse
      . to (readMaybe @Int . Text.unpack) . traverse
    pure $ Laptop
      { laptopName = name
      , laptopPrice = price
      , laptopDescription = description
      , laptopStars = stars
      }

data Args = Args
  { argsOutFile :: FilePath
  }

parseArgs :: IO Args
parseArgs = customExecParser parserPrefs $ info (args <**> helper) mempty
  where
    parserPrefs = prefs showHelpOnEmpty
    args = Args
      <$> do strOption $ short 'o' <> long "out" <> metavar "OUT" <> help "JSON file to write results to"

main :: IO ()
main = do
  Args{..} <- parseArgs
  let pages = [1..20]
  -- if we had more than 20 pages, only open 100 at a time to prevent too many
  -- open connections
  let chunksOf n = takeWhile (not . null) . fmap (take n) . iterate (drop n)
  scores <- fmap concat $ for (chunksOf 100 pages) $ \pageRange -> do
    fmap concat $ forConcurrently pageRange $ \page -> do
      res <- fmap scrapePage <$> getPage page
      case res of
        Left err -> do
          putStrLn $ "Error when fetching page " <> show page <> ": " <> err
          pure []
        Right Nothing -> do
          putStrLn $ "Error when scraping data from page " <> show page
          pure []
        Right (Just scores) -> pure scores
  Aeson.encodeFile argsOutFile scores

