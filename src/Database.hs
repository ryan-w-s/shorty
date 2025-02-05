{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database 
    ( initDB
    , runDB
    , SqlBackend
    , Migration
    ) where

import Control.Monad.Logger (runStderrLoggingT, LoggingT, logInfo)
import Control.Monad.Reader (ReaderT)
import Database.Persist.Sqlite
import Models (migrateAll)
import Data.Text (Text)
import System.Directory (doesFileExist)

-- Database file path
dbPath :: Text
dbPath = "shorty.db"

-- Initialize the database and run migrations if needed
initDB :: IO ()
initDB = do
    dbExists <- doesFileExist "shorty.db"
    runStderrLoggingT $ withSqlitePool dbPath 10 $ \pool -> do
        -- Run migrations with proper logging
        flip runSqlPool pool $ do
            if dbExists 
                then $(logInfo) "Database exists, checking for migrations..."
                else $(logInfo) "Database does not exist. Creating..."
            $(logInfo) "Running migrations..."
            runMigration migrateAll
            $(logInfo) "Migrations complete."

-- Run a database action with logging
runDB :: ReaderT SqlBackend (LoggingT IO) a -> IO a
runDB action = runStderrLoggingT $ withSqlitePool dbPath 10 $ \pool ->
    runSqlPool action pool 