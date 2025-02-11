{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actions
    ( -- * URL Operations
      createUrl
    , getUrlById
    , getAllUrls
    , incrementClicks
    , UrlInfo(..)
    -- * Base62 Conversion
    , toBase62
    , fromBase62
    ) where

import Database.Persist
import Database.Persist.Sqlite
import Data.Time (UTCTime, getCurrentTime)
import Data.Char (ord)
import Data.Int (Int64)
import Models
import Database (runDB)

-- | Information about a URL with its shortcode
data UrlInfo = UrlInfo
    { urlId :: Integer      -- The numeric ID
    , originalUrl :: String
    , shortCode :: String   -- base62 representation of the id
    , createdAt :: UTCTime
    , clicks :: Int
    } deriving (Show, Eq)

-- | Convert a database Url to UrlInfo
toUrlInfo :: Entity Url -> UrlInfo
toUrlInfo (Entity key url) = UrlInfo
    { urlId = fromIntegral (fromSqlKey key :: Int64)
    , originalUrl = urlOriginalUrl url
    , shortCode = toBase62 $ fromIntegral (fromSqlKey key :: Int64)
    , createdAt = urlCreatedAt url
    , clicks = urlClicks url
    }

-- | Create a new URL shortcut
createUrl :: String -> IO UrlInfo
createUrl url = do
    now <- getCurrentTime
    let newUrl = Url
            { urlOriginalUrl = url
            , urlCreatedAt = now
            , urlClicks = 0
            }
    key <- runDB $ insert newUrl
    pure $ toUrlInfo (Entity key newUrl)

-- | Get a URL by its ID
getUrlById :: Integer -> IO (Maybe UrlInfo)
getUrlById n = do
    let key = toSqlKey (fromIntegral n) :: Key Url
    result <- runDB $ get key
    pure $ fmap (toUrlInfo . Entity key) result

-- | Get all URLs
getAllUrls :: IO [UrlInfo]
getAllUrls = do
    entities <- runDB $ selectList [] [Desc UrlCreatedAt]
    pure $ map toUrlInfo entities

-- | Increment the click count for a URL and return the updated info
incrementClicks :: Integer -> IO (Maybe UrlInfo)
incrementClicks n = do
    let key = toSqlKey (fromIntegral n) :: Key Url
    result <- runDB $ do
        -- Get the current URL
        maybeUrl <- get key
        case maybeUrl of
            Nothing -> pure Nothing
            Just url -> do
                -- Update the click count
                let newClicks = urlClicks url + 1
                update key [UrlClicks =. newClicks]
                -- Get the updated URL
                updatedUrl <- get key
                pure $ fmap (toUrlInfo . Entity key) updatedUrl
    pure result

-- Base62 conversion helpers
base62Chars :: String
base62Chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Convert an Integer to base62 string
toBase62 :: Integer -> String
toBase62 0 = [head base62Chars]
toBase62 num = reverse $ go num
  where
    go 0 = []
    go x = base62Chars !! fromIntegral (x `mod` 62) : go (x `div` 62)

-- | Convert a base62 string back to Integer
fromBase62 :: String -> Maybe Integer
fromBase62 = fmap (foldl (\acc x -> acc * 62 + x) 0) . mapM charToValue
  where
    charToValue c
        | c >= '0' && c <= '9' = Just $ fromIntegral $ ord c - ord '0'
        | c >= 'a' && c <= 'z' = Just $ fromIntegral $ ord c - ord 'a' + 10
        | c >= 'A' && c <= 'Z' = Just $ fromIntegral $ ord c - ord 'A' + 36
        | otherwise = Nothing 