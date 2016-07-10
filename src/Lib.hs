{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib
    ( AppM
    , runAppM
    , Dog(..)
    , DogId
    , Cat(..)
    , CatId
    , initializeDatabase
    ) where

import           Database.Persist.Sqlite (ConnectionPool, SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.TH     (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


type AppM = SqlPersistT IO

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Cat
    head String
    tail String
    deriving Show
Dog
    name String
    age  Int
    deriving Show
|]

initializeDatabase :: ConnectionPool -> IO ()
initializeDatabase pool = runSqlPool (runMigration migrateAll) pool

runAppM :: ConnectionPool -> AppM a -> IO a
runAppM pool appM = runSqlPool appM pool

