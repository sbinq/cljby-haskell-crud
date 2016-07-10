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
    , encodeEntity
    ) where


import           Data.Aeson              (ToJSON (..), Value (..))
import           Data.Aeson.TH           (deriveJSON)
import qualified Data.HashMap.Strict     as HM
import           Database.Persist        (Entity (..))
import           Database.Persist.Sqlite (ConnectionPool, SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.TH     (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import           Utils

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

$(deriveJSON (aesonOptions "cat") ''Cat)
$(deriveJSON (aesonOptions "dog") ''Dog)

initializeDatabase :: ConnectionPool -> IO ()
initializeDatabase pool = runSqlPool (runMigration migrateAll) pool

runAppM :: ConnectionPool -> AppM a -> IO a
runAppM pool appM = runSqlPool appM pool



-- reusable stuff

encodeEntity :: ToJSON val => Entity val -> Value
encodeEntity (Entity key value) =
  case toJSON value of
    Object o -> Object $ HM.insert "id" (toJSON key) o
    _        -> error "entity type not supported (it is not an object)" -- TODO: bad error message - all details lost
