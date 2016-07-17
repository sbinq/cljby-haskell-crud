{-# LANGUAGE ConstraintKinds            #-}
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
    , encodeEntity
    , UpdateOrInsert(..)
    , updateOrInsert
    , findById
    ) where

import           Control.Monad        (return)
import           Data.Aeson           (FromJSON (..), ToJSON (..), Value (..), (.:?))
import qualified Data.HashMap.Strict  as HM
import           Data.Int             (Int64)
import           Database.Persist     (Entity (..), Key (..), get, insert, replace)
import           Database.Persist.Sql (ConnectionPool, SqlBackend, SqlPersistT, ToBackendKey, runSqlPool, toSqlKey)

type AppM = SqlPersistT IO

runAppM :: ConnectionPool -> AppM a -> IO a
runAppM pool appM = runSqlPool appM pool


data UpdateOrInsert val = UpdateOrInsert { uoiEntityId    :: Maybe Int64
                                         , uoiEntityValue :: val
                                         } deriving (Show, Eq)

instance FromJSON val => FromJSON (UpdateOrInsert val) where
  parseJSON (Object v) = UpdateOrInsert <$> v .:? "id" <*> (parseJSON $ Object $ HM.delete "id" v)
  parseJSON other      = fail ("object expected, but received " ++ show other)


type IsEntity val = ToBackendKey SqlBackend val


updateOrInsert :: IsEntity val => UpdateOrInsert val -> AppM (Maybe (Key val))
updateOrInsert uoi =
  case (uoiEntityId uoi) of
    Just entityId -> do let k = toSqlKey entityId
                         -- seems like no way to just receive number of updated/matched rows after update (shame on you Haskell!),
                         -- this is kind of sad, so doing one more query just to check if there is something for our key in database
                        maybeVal <- get k
                        case maybeVal of
                          Nothing -> return Nothing
                          Just _  -> do replace k $ uoiEntityValue uoi
                                        return $ Just k
    Nothing       -> do k <- insert $ uoiEntityValue uoi
                        return $ Just k


findById :: IsEntity val => Int64 -> AppM (Maybe (Entity val))
findById entityId = do
  let k = toSqlKey entityId
  maybeVal <- get k
  return $ fmap (Entity k) maybeVal


encodeEntity :: (ToJSON val, Show val) => Entity val -> Value
encodeEntity (Entity key value) =
  case toJSON value of
    Object o -> Object $ HM.insert "id" (toJSON key) o
    other    -> error ("unsupported type: " ++ show value ++ " serialized as " ++ show other)
