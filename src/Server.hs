{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server
    ( startServer
    ) where

import Web.Scotty
import Data.Text.Lazy (Text)
import Control.Exception (try, SomeException)
import Lucid (renderText)
import Lib (createUrl)
import View (newUrlForm, errorPage, shortcutCreated)

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

-- | Start the web server
startServer :: IO ()
startServer = scotty 3000 routes 