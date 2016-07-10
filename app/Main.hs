{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Logger      (runStdoutLoggingT)
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson                (ToJSON)
import           Data.Int                  (Int64)
import           Database.Persist          (Entity)
import           Database.Persist.Sqlite   (createSqlitePool)
import           Network.HTTP.Types        (status404)
import           Web.Spock.Safe            (ActionT, get, json, post, runSpock, setStatus, spockT, var, (<//>))

import           Lib                       (AppM, Cat, Dog, encodeEntity, findById, initializeDatabase, runAppM)


main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "/tmp/sample.db" 4
  initializeDatabase pool

  runSpock 8080 $ spockT (runAppM pool) $ do

    get ("/cat" <//> var ) $ \(catId :: Int64) -> do
      cat <- lift $ findById catId
      entityOr404 (cat :: Maybe (Entity Cat))

    post "/cat" $ undefined

    get ("/dog" <//> var) $ \(dogId :: Int64) -> do
      dog <- lift $ findById dogId
      entityOr404 (dog :: Maybe (Entity Dog))

    post "/dog" $ undefined


entityOr404 :: (ToJSON ent, Show ent) => Maybe (Entity ent) -> ActionT AppM ()
entityOr404 maybeEntity =
  case maybeEntity of
    Nothing     -> setStatus status404
    Just entity -> json $ encodeEntity entity
