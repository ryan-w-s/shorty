{-# LANGUAGE OverloadedStrings #-}

module View
    ( pageStyle
    , newUrlForm
    , errorPage
    , shortcutCreated
    ) where

import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Lucid
import Lib (UrlInfo(..))

-- | CSS styles for the application
pageStyle :: T.Text
pageStyle = mconcat
    [ "body { font-family: Arial, sans-serif; max-width: 800px; margin: 2em auto; padding: 0 1em; }"
    , "form { display: flex; gap: 1em; margin: 2em 0; }"
    , "input[type=url] { flex-grow: 1; padding: 0.5em; }"
    , "input[type=submit] { padding: 0.5em 1em; cursor: pointer; }"
    , "a { color: #0066cc; text-decoration: none; }"
    , "a:hover { text-decoration: underline; }"
    ]

-- | Base HTML template
template :: T.Text -> Html () -> Html ()
template title' content = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title')
        style_ pageStyle
    body_ content

-- | Form for creating new shortcuts
newUrlForm :: Html ()
newUrlForm = template "Create New URL Shortcut" $ do
    h1_ "Create New URL Shortcut"
    form_ [method_ "post", action_ "/new"] $ do
        input_ [type_ "url", name_ "url", placeholder_ "Enter URL to shorten", required_ "required"]
        input_ [type_ "submit", value_ "Create Shortcut"]

-- | Error page
errorPage :: Text -> Html ()
errorPage msg = template "Error" $ do
    h1_ "Error"
    p_ $ toHtml msg

-- | Success page after creating a shortcut
shortcutCreated :: UrlInfo -> Html ()
shortcutCreated info = template "Shortcut Created" $ do
    h1_ "Shortcut Created"
    p_ $ do
        "Original URL: "
        a_ [href_ $ T.pack $ originalUrl info] $ toHtml $ originalUrl info
    p_ $ do
        "Shortcut: "
        let shortUrl = T.pack $ "/go/" <> shortCode info
        a_ [href_ shortUrl] $ toHtml $ T.unpack shortUrl 