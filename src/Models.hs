{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeOperators             #-}

module Models 
    ( -- * URL Entity
      Url(..)
    , UrlId
    , migrateAll
    -- * Generated Types
    , EntityField(..)
    , Unique(..)
    , Key(..)
    ) where

import Database.Persist.TH
import Database.Persist.Sql
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Url
    originalUrl String
    createdAt UTCTime
    clicks Int default=0
    UniqueOriginalUrl originalUrl
    deriving Show
|] 