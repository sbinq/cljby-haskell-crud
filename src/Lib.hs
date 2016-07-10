{-# LANGUAGE FlexibleContexts           #-}
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
    , UpdateOrInsert(..)
    , updateOrInsert
    , findById
    ) where

-- TODO: consider using Sql - not Sqlite - a bit more reusable

import           Control.Monad           (return)
import           Data.Aeson              (FromJSON (..), ToJSON (..), Value (..), (.:?))
import           Data.Aeson.TH           (deriveJSON)
import qualified Data.HashMap.Strict     as HM
import           Data.Int                (Int64)
import           Database.Persist        (Entity (..), Key (..), get, insert, replace)
import           Database.Persist.Sqlite (ConnectionPool, SqlBackend, SqlPersistT, ToBackendKey, runMigration,
                                          runSqlPool, toSqlKey)
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

encodeEntity :: (ToJSON val, Show val) => Entity val -> Value
encodeEntity (Entity key value) =
  case toJSON value of
    Object o -> Object $ HM.insert "id" (toJSON key) o
    other    -> error ("unsupported type: " ++ show value ++ " serialized as " ++ show other)


data UpdateOrInsert val = UpdateOrInsert { uoiEntityId    :: Maybe Int64
                                         , uoiEntityValue :: val
                                         }

instance FromJSON val => FromJSON (UpdateOrInsert val) where
  parseJSON (Object v) = UpdateOrInsert <$> v .:? "id" <*> (parseJSON $ Object $ HM.delete "id" v)
  parseJSON other      = fail ("object expected, but received " ++ show other)


updateOrInsert :: ToBackendKey SqlBackend val => UpdateOrInsert val -> AppM (Key val)
updateOrInsert uoi =
  case (uoiEntityId uoi) of
    Just entityId -> do let k = toSqlKey entityId
                        replace k $ uoiEntityValue uoi -- TODO: handle wrong key case - return AppM (Maybe (Key val)) or smth
                        return k
    Nothing       -> insert $ uoiEntityValue uoi


findById :: ToBackendKey SqlBackend val => Int64 -> AppM (Maybe (Entity val))
findById entityId = do
  let k = toSqlKey entityId
  maybeVal <- get k
  return $ fmap (Entity k) maybeVal
