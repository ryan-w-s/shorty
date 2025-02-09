{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( startServer
    ) where

import Web.Scotty
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Control.Exception (try, SomeException)
import Lucid (renderText)
import Lib (createUrl, getUrlById, fromBase62, UrlInfo(..))
import View (newUrlForm, errorPage, shortcutCreated, urlMetadata)

-- | Application routes
routes :: ScottyM ()
routes = do
    -- Show homepage
    get "/" $ do
        text ("URL Shortener Service" :: Text)

    -- Show form for creating new shortcuts
    get "/new" $ do
        html $ renderText newUrlForm

    -- Handle form submission
    post "/new" $ do
        url <- formParam "url"
        result <- liftAndCatchIO $ try $ createUrl url
        case result of
            Left (_ :: SomeException) -> 
                html $ renderText $ errorPage "Invalid URL or database error"
            Right urlInfo -> 
                html $ renderText $ shortcutCreated urlInfo

    -- View URL metadata
    get "/get/:code" $ do
        code <- captureParam "code"
        case fromBase62 code of
            Nothing -> 
                html $ renderText $ errorPage "Invalid shortcode format"
            Just shortUrlId -> do
                maybeUrl <- liftAndCatchIO $ getUrlById shortUrlId
                case maybeUrl of
                    Nothing ->
                        html $ renderText $ errorPage "Shortcode not found"
                    Just urlInfo ->
                        html $ renderText $ urlMetadata urlInfo

    -- Redirect to original URL
    get "/go/:code" $ do
        code <- captureParam "code"
        case fromBase62 code of
            Nothing -> 
                html $ renderText $ errorPage "Invalid shortcode format"
            Just shortUrlId -> do
                maybeUrl <- liftAndCatchIO $ getUrlById shortUrlId
                case maybeUrl of
                    Nothing ->
                        html $ renderText $ errorPage "Shortcode not found"
                    Just urlInfo ->
                        redirect $ TL.pack $ originalUrl urlInfo

-- | Start the web server
startServer :: IO ()
startServer = scotty 3000 routes 