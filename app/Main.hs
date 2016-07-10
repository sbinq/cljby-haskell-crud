{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Logger    (runStdoutLoggingT)
import           Database.Persist.Sqlite (createSqlitePool)
import           Web.Spock.Safe          (get, post, runSpock, spockT, text, var, (<//>))

import           Lib                     (runAppM)


main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "/tmp/sample.db" 4

  runSpock 8080 $ spockT (runAppM pool) $ do

    get ("/cat" <//> var ) $ \(catId :: Int) -> do
      text $ "hello"

    post "/cat" $ do
      undefined

    get ("/dog" <//> var) $ \(dogId :: Int) -> do
      undefined

    post "/dog" $ do
      undefined

