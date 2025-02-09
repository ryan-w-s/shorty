{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (initDB)
import Server (startServer)

main :: IO ()
main = do
    -- Initialize the database
    initDB
    
    -- Start the web server
    startServer
