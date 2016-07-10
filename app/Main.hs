{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Spock.Safe (runSpock, spockT, get, post, (<//>), text, var)


main :: IO ()
main = do
  runSpock 8080 $ spockT id $ do

    get ("/cat" <//> var ) $ \(catId :: Int) -> do
      text $ "hello"

    post "/cat" $ do
      undefined

    get ("/dog" <//> var) $ \(dogId :: Int) -> do
      undefined

    post "/dog" $ do
      undefined

