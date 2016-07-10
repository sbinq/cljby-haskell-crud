{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Logger      (runStdoutLoggingT)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson                (ToJSON, object, (.=))
import           Data.Int                  (Int64)
import           Database.Persist          (Entity, Key)
import           Database.Persist.Sqlite   (createSqlitePool)
import           Network.HTTP.Types        (status400, status404)
import           Web.Spock.Safe            (ActionT, get, json, jsonBody', post, runSpock, setStatus, spockT, text, var,
                                            (<//>))

import           Lib                       (AppM, Cat, Dog, UpdateOrInsert, encodeEntity, findById, initializeDatabase,
                                            runAppM, updateOrInsert)


main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "/tmp/sample.db" 4
  initializeDatabase pool

  runSpock 8080 $ spockT (runAppM pool) $ do

    get ("/cat" <//> var ) $ \(catId :: Int64) -> do
      cat <- lift $ findById catId
      entityOr404 (cat :: Maybe (Entity Cat))

    post "/cat" $ do
      cmd <- jsonBody'
      k <- lift $ updateOrInsert (cmd :: UpdateOrInsert Cat)
      entityIdOr400 k

    get ("/dog" <//> var) $ \(dogId :: Int64) -> do
      dog <- lift $ findById dogId
      entityOr404 (dog :: Maybe (Entity Dog))

    post "/dog" $ do
      cmd <- jsonBody'
      k <- lift $ updateOrInsert (cmd :: UpdateOrInsert Dog)
      entityIdOr400 k


entityOr404 :: (ToJSON ent, Show ent) => Maybe (Entity ent) -> ActionT AppM ()
entityOr404 maybeEntity =
  case maybeEntity of
    Nothing     -> do setStatus status404
                      text "Entity not found"
    Just entity -> json $ encodeEntity entity

entityIdOr400 :: ToJSON (Key ent) => Maybe (Key ent) -> ActionT AppM ()
entityIdOr400 maybeId =
  case maybeId of
    Nothing       -> do setStatus status400
                        text "Trying to update non-existing entity"
    Just entityId -> json $ object [ "id" .= entityId]
