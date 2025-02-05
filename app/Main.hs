{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (initDB)
import Web.Scotty
import Data.Text.Lazy (Text)

main :: IO ()
main = do
    -- Initialize the database
    initDB
    
    -- Start the web server
    scotty 3000 $ do
        get "/" $ do
            text ("URL Shortener Service" :: Text)
