{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sqlite (SqlBackend, createSqlitePool)
import Data.Pool (Pool)
import Web.Spock.Safe (runSpock, spockT, get, post, (<//>), text, var)

main :: IO ()
main = do
  (pool :: Pool SqlBackend) <- runStdoutLoggingT $ createSqlitePool "/tmp/sample.db" 4

  runSpock 8080 $ spockT id $ do

    get ("/cat" <//> var ) $ \(catId :: Int) -> do
      text $ "hello"

    post "/cat" $ do
      undefined

    get ("/dog" <//> var) $ \(dogId :: Int) -> do
      undefined

    post "/dog" $ do
      undefined

