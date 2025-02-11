{-# LANGUAGE OverloadedStrings #-}

module View
    ( pageStyle
    , newUrlForm
    , errorPage
    , shortcutCreated
    , urlMetadata
    , homePage
    ) where

import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Lucid
import Lib (UrlInfo(..))
import qualified Data.Foldable as F

-- | CSS styles for the application
pageStyle :: T.Text
pageStyle = mconcat
    [ "body { font-family: Arial, sans-serif; max-width: 800px; margin: 2em auto; padding: 0 1em; }"
    , "form { display: flex; gap: 1em; margin: 2em 0; }"
    , "input[type=url] { flex-grow: 1; padding: 0.5em; }"
    , "input[type=submit] { padding: 0.5em 1em; cursor: pointer; }"
    , "a { color: #0066cc; text-decoration: none; }"
    , "a:hover { text-decoration: underline; }"
    , ".url-list { list-style: none; padding: 0; }"
    , ".url-item { border-bottom: 1px solid #eee; padding: 1em 0; }"
    , ".url-stats { color: #666; font-size: 0.9em; margin-top: 0.5em; }"
    , ".create-new { display: inline-block; margin: 1em 0; padding: 0.5em 1em; background: #0066cc; color: white; border-radius: 4px; }"
    , ".create-new:hover { background: #0052a3; text-decoration: none; }"
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
    p_ $ do
        "View metadata: "
        let metadataUrl = T.pack $ "/get/" <> shortCode info
        a_ [href_ metadataUrl] "View statistics and details"

-- | Display URL metadata
urlMetadata :: UrlInfo -> Html ()
urlMetadata info = template "URL Information" $ do
    h1_ "URL Information"
    div_ [class_ "metadata"] $ do
        p_ $ do
            strong_ "Original URL: "
            a_ [href_ $ T.pack $ originalUrl info] $ toHtml $ originalUrl info
        p_ $ do
            strong_ "Shortcode: "
            code_ $ toHtml $ shortCode info
        p_ $ do
            strong_ "Created: "
            toHtml $ show $ createdAt info
        p_ $ do
            strong_ "Times clicked: "
            toHtml $ show $ clicks info
        p_ $ do
            strong_ "Go to URL: "
            let goUrl = T.pack $ "/go/" <> shortCode info
            a_ [href_ goUrl] $ toHtml $ T.unpack goUrl 

-- | Homepage with list of URLs
homePage :: [UrlInfo] -> Html ()
homePage urls = template "URL Shortener" $ do
    h1_ "URL Shortener"
    a_ [class_ "create-new", href_ "/new"] "Create New Shortcut"
    
    if null urls
        then p_ "No URLs have been shortened yet."
        else ul_ [class_ "url-list"] $ do
            F.for_ urls $ \info -> li_ [class_ "url-item"] $ do
                div_ $ do
                    "Original: "
                    a_ [href_ $ T.pack $ originalUrl info] $ 
                        toHtml $ truncateUrl $ originalUrl info
                div_ $ do
                    "Shortcut: "
                    let shortUrl = T.pack $ "/go/" <> shortCode info
                    a_ [href_ shortUrl] $ toHtml $ T.unpack shortUrl
                div_ [class_ "url-stats"] $ do
                    a_ [href_ $ T.pack $ "/get/" <> shortCode info] $ do
                        toHtml $ show $ clicks info
                        " clicks • Created "
                        toHtml $ show $ createdAt info

-- | Helper to truncate long URLs for display
truncateUrl :: String -> String
truncateUrl url
    | length url <= 50 = url
    | otherwise = take 47 url <> "..." 