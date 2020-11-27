{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.Char (isUpper, toLower)
import Data.Fixed (Centi)
import Data.Traversable (for)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Control.Concurrent.Async (forConcurrently)
import Control.Lens
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Req ((=:), (/:))
import qualified Network.HTTP.Req as Req
import qualified Text.Xml.Lens as X

getPage :: Int -> IO (Either String ByteString)
getPage p = Req.runReq httpCfg $ do
  r <- Req.req Req.GET baseURI Req.NoReqBody Req.bsResponse $ "page" =: p
  let statusCode = Req.responseStatusCode r
  if statusCode == 200
    then pure $ Right $ Req.responseBody r
    else pure $ Left $ "Unexpected error code: " <> show statusCode
  where
    httpCfg = Req.defaultHttpConfig
    baseURI = Req.https "www.webscraper.io" /: "test-sites" /: "e-commerce" /: "static" /: "computers" /: "laptops"

data Laptop = Laptop
  { laptopName :: !Text
  , laptopPrice :: !Centi
  , laptopDescription :: !Text
  , laptopStars :: !Int
  }
  deriving (Show, Generic)

toSnake :: String -> String
toSnake s = s & _head %~ toLower & _tail %~ snakeRest
  where
    snakeRest "" = ""
    snakeRest (c:cs)
      | isUpper c = '_' : toLower c : snakeRest cs
      | otherwise = c : snakeRest cs

instance Aeson.ToJSON Laptop where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = toSnake . drop (Text.length "laptop")
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

data ChanMsg a = Msg a | CloseChan

worker :: (a -> IO ()) -> IO (Chan (ChanMsg a))
worker f = do
  c <- newChan
  let go = readChan c >>= \case
        CloseChan -> pure ()
        Msg x     -> f x *> go
  _ <- forkIO go
  pure c

send :: Chan (ChanMsg a) -> a -> IO ()
send c x = writeChan c (Msg x)

close :: Chan (ChanMsg a) -> IO ()
close c = writeChan c CloseChan

withWorker :: (a -> IO ()) -> (Chan (ChanMsg a) -> IO b) -> IO b
withWorker f = bracket (worker f) close

main :: IO ()
main = do
  [outFile] <- getArgs
  let pages = [1..20]
  -- if we had more than 20 pages, only open 100 at a time to prevent too many
  -- open connections
  let chunksOf n = takeWhile (not . null) . fmap (take n) . iterate (drop n)
  scores <- withWorker putStrLn $ \logger -> do
    fmap concat $ for (chunksOf 100 pages) $ \pageRange -> do
      fmap concat $ forConcurrently pageRange $ \page -> do
        res <- fmap scrapePage <$> getPage page
        case res of
          Left err -> do
            send logger $ "Error when fetching page " <> show page <> ": " <> err
            pure []
          Right Nothing -> do
            send logger $ "Error when scraping data from page " <> show page
            pure []
          Right (Just scores) -> pure scores
  Aeson.encodeFile outFile scores

