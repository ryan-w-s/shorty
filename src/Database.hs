{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database 
    ( initDB
    , runDB
    , SqlBackend
    , Migration
    , setDBPath
    ) where

import Control.Monad.Logger (runStderrLoggingT, LoggingT, logInfo)
import Control.Monad.Reader (ReaderT)
import Database.Persist.Sqlite
import Models (migrateAll)
import Data.Text (Text)
import System.Directory (doesFileExist)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import System.IO.Unsafe (unsafePerformIO)

-- | Database file path reference
{-# NOINLINE dbPathRef #-}
dbPathRef :: IORef Text
dbPathRef = unsafePerformIO $ newIORef "shorty.db"

-- | Set the database path
setDBPath :: Text -> IO ()
setDBPath = writeIORef dbPathRef

-- | Get the current database path
getDBPath :: IO Text
getDBPath = readIORef dbPathRef

-- Initialize the database and run migrations if needed
initDB :: IO ()
initDB = do
    dbPath <- getDBPath
    dbExists <- doesFileExist (show dbPath)
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
runDB action = do
    dbPath <- getDBPath
    runStderrLoggingT $ withSqlitePool dbPath 10 $ \pool ->
        runSqlPool action pool 