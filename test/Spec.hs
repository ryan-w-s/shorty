{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Control.Exception (bracket_, try, SomeException)
import System.Directory (removeFile, doesFileExist)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)

import Actions
import Models
import Database (setDBPath)

-- | Counter for generating unique database names
{-# NOINLINE dbCounter #-}
dbCounter :: IORef Int
dbCounter = unsafePerformIO $ newIORef 0

-- | Create a temporary database, run the test, then clean up
withTestDB :: IO a -> IO a
withTestDB action = do
    -- Generate a unique database name
    counter <- readIORef dbCounter
    writeIORef dbCounter (counter + 1)
    let dbPath = "test" <> T.pack (show counter) <> ".db"
    let dbPathStr = T.unpack dbPath
    
    -- Remove any existing database with this name
    fileExists <- doesFileExist dbPathStr
    when fileExists $ removeFile dbPathStr
    
    -- Set the database path for the test
    setDBPath dbPath
    
    -- Run the test with a fresh database
    bracket_
        (runStderrLoggingT $ withSqlitePool dbPath 1 $ \pool -> 
            runSqlPool (runMigration migrateAll) pool)
        (removeFile dbPathStr)
        action

main :: IO ()
main = hspec $ do
    describe "Base62" $ do
        it "converts 0 to '0'" $ do
            toBase62 0 `shouldBe` "0"

        it "converts numbers to base62 and back" $ property $
            \n -> n >= 0 ==> fromBase62 (toBase62 n) == Just n

        it "handles some known cases" $ do
            toBase62 1000 `shouldBe` "g8"
            toBase62 999999 `shouldBe` "4c91"
            fromBase62 "g8" `shouldBe` Just 1000
            fromBase62 "4c91" `shouldBe` Just 999999

        it "rejects invalid characters" $ do
            fromBase62 "!@#" `shouldBe` Nothing
            fromBase62 "-123" `shouldBe` Nothing

    describe "URL Operations" $ do
        it "creates and retrieves a URL" $ withTestDB $ do
            url <- createUrl "https://example.com"
            retrieved <- getUrlById (urlId url)
            retrieved `shouldBe` Just url

        it "returns Nothing for non-existent URLs" $ withTestDB $ do
            retrieved <- getUrlById 999999
            retrieved `shouldBe` Nothing

        it "lists URLs in reverse chronological order" $ withTestDB $ do
            -- Create a few URLs
            _ <- createUrl "https://example1.com"
            _ <- createUrl "https://example2.com"
            _ <- createUrl "https://example3.com"
            
            -- Get all URLs
            urls <- getAllUrls
            
            -- Check order and content
            map originalUrl urls `shouldBe` 
                ["https://example3.com", "https://example2.com", "https://example1.com"]

        it "enforces unique URLs" $ withTestDB $ do
            -- Create first URL
            _ <- createUrl "https://example.com"
            
            -- Try to create the same URL again
            result <- try $ createUrl "https://example.com"
            case result of
                Left (_ :: SomeException) -> 
                    pure () -- Expected exception
                Right _ -> expectationFailure "Expected unique constraint violation"
